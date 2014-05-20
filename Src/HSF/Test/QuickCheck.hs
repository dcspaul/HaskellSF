{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.QuickCheck
	( checkWithScala, checkWithOCaml, checkWithHP
	) where

import HSF.Options
import HSF.Parser
import HSF.Eval
import HSF.Errors
import HSF.Test.RunScalaVersion

import Data.List (intercalate,nub)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad
import System.FilePath.Posix (isAbsolute,(</>))
import Control.Monad.State

{--
    See: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

	Gen a is a generator for type a
    "arbitrary" is generator
    
	class Arbitrary a where
         arbitrary :: Gen a

    the basic generator is choose(lower,upper) which returns a value between upper & lower (inclusive)

data Identifier = Identifier [Char] deriving(Eq,Show)
data Reference = Reference [Identifier] deriving(Eq,Show)
data Body = Body [Assignment] deriving(Eq,Show)
data BasicValue = BoolValue Bool | NumValue Integer | StringValue [Char] | NullValue
                | DataRef [Identifier] | Vector [BasicValue] deriving(Eq,Show)
data Value = BasicValue BasicValue | LinkValue Reference | ProtoValue [Prototype] deriving(Eq,Show)
data Assignment = Assignment Reference Value deriving(Eq,Show)
data Prototype = RefProto Reference | BodyProto Body deriving(Eq,Show)

--}


{------------------------------------------------------------------------------
    arbitrary items
------------------------------------------------------------------------------}

-- TODO: not yet supporting vectors ?

-- identifiers are arbitrary (lowercase) letters

instance Arbitrary Identifier where
	arbitrary =  oneof (map (return . Identifier) ids)
		where ids = map (:[]) ['a' .. 'z']

-- we need to have separate generators for references, depending on how
-- the references are used. so we don't define an Arbitrary instance -
-- use one of the following functions instead:

-- a reference appearing on the LHS of an assignment is either a single identifier
-- or a compound reference
-- we choose single identifiers using the arbitrary identifier code
-- we just generate a marker for compound references so we can substitute them
-- with something valid later on
-- TODO: think about the frequencies later

arbitraryLHSRef :: Gen Reference
arbitraryLHSRef = frequency [(2,singleIdentifier),(1,return dummyRef)]
	where
		dummyRef = (Reference [Identifier "LHS"])
		singleIdentifier = do
			id <- arbitrary
			return (Reference [id])

-- a reference appearing on the RHS of an assignment must refer to something
-- we just generate a marker so we can substitute them with something valid later on

arbitraryRHSRef :: Gen Reference
arbitraryRHSRef = do
	return (Reference [Identifier "RHS"])

-- a body is a non-empty list of assignments, of the appropriate size
-- TODO: think about the frequencies later
-- TODO: should we allow it to be empty?

instance Arbitrary Body where
	arbitrary = sized body' where
		body' n
			| n<=1 = liftM Body ((resize 1) arbitrary)
			| n>1  = liftM Body ((resize n) arbitrary)

-- arbitrary basic values
-- data references are not evaluated, so we can just generate an arbitrary (non-empty)
-- list of Identifiers
	
instance Arbitrary BasicValue where
	arbitrary = oneof
		[ liftM BoolValue arbitrary
		, liftM NumValue (return 1234)
		, liftM StringValue (return "string")
		, do
			first <- arbitrary
			rest <- (resize 3) arbitrary
			return (DataRef (first:rest))
		]

-- a value is a BasicValue or an (RHS) Reference, or a list of Prototypes

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM BasicValue arbitrary
		, liftM LinkValue arbitraryRHSRef
		, do
			first <- arbitrary
			rest <- (resize 3) arbitrary
			return (ProtoValue (first:rest))
		]

-- assignment is an arbitrary (LHS) Reference and an arbitrary Value

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitraryLHSRef arbitrary

-- prototype is an arbitrary Body, or an arbitrary (RHS) Reference

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitrary
		, liftM RefProto arbitraryRHSRef
		]

{------------------------------------------------------------------------------
    top-level source
------------------------------------------------------------------------------}

-- the top-level body is slightly different, because ...
-- the aim here is to generate a list of assignments with at least one sfConfig
-- TODO: I guess you might want to test configurations with no sfconfig
-- so we could make this randomly something else ....
-- you might also want to test an empty top level?

newtype SfSource = SfSource String deriving(Eq)

instance Show SfSource where
	show (SfSource s) = id s

data SFConfig = SFConfig [Assignment] deriving(Eq,Show)

instance Arbitrary SFConfig where
	arbitrary = do
		left <- ((resize 3) arbitrary)
		sfConfig <- (resize 3) arbitrary
		let a = Assignment (Reference [Identifier "sfConfig"]) sfConfig
		right <- ((resize 3) arbitrary)
		return (evalRefs (left ++ [a] ++ right))

renderConfig :: SFConfig -> String
renderConfig = render

instance Arbitrary SfSource where
	arbitrary = liftM SfSource $ liftM renderConfig arbitrary

instance ParseItem SFConfig where
	render (SFConfig as) = intercalate "\n" (map render as)

-- see: http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

{------------------------------------------------------------------------------
    substitute references
------------------------------------------------------------------------------}


data SymTab = SymTab String [SymTab]

top = SymTab "top" []

-- process a list of assignments by
-- replacing the LHS and RHS "placeholders" with arbitrary, valid references
evalRefs :: [Assignment] -> SFConfig 
evalRefs as = (SFConfig as')
	where (_,(Body as')) = subBodyRef (top,(Body [])) (Body as)

-- evaluate list of assignments (left to right) 
subBodyRef :: (SymTab,Body) -> Body -> (SymTab,Body)
subBodyRef (t,body) (Body as) = foldl subAssignRef (t,body) as

-- evaluate one assignment
subAssignRef :: (SymTab,Body) -> Assignment -> (SymTab,Body)
subAssignRef (t,body) (Assignment r v) =
	let
		(r',t') = case r of
			(Reference [Identifier "LHS"]) -> subLHSRef t
			otherwise -> (r,t)
		(v',t'') = case v of
			(ProtoValue ps) -> subProtoListRef t' ps
			(LinkValue (Reference [Identifier "RHS"])) -> ( (subRHSLinkRef t'), t' )
			otherwise -> (v,t')
	in ( t'', (appendToBody body (Assignment r' v')))
		where appendToBody (Body as) a = Body (as ++ [a])

subLHSRef :: SymTab -> (Reference,SymTab)
subLHSRef t = ( Reference [Identifier "NEWREF"] , t )
-- invent a reference
-- add it to the symbol table

subRHSProtoRef :: SymTab -> Reference
subRHSProtoRef t = Reference [Identifier "PROTOREF"]
-- invent a reference

subRHSLinkRef :: SymTab -> Value
subRHSLinkRef t = LinkValue ( Reference [Identifier "LINKREF"] )
-- invent a reference

subProtoListRef :: SymTab -> [Prototype] -> (Value,SymTab)
subProtoListRef t ps = ((ProtoValue ps'),t')
	where (t',ps') = foldl subProtoRef (t,[]) ps

subProtoRef :: (SymTab,[Prototype]) -> Prototype -> (SymTab,[Prototype])
subProtoRef (t,ps) p = case p of
	(RefProto (Reference [Identifier "RHS"])) -> ( t, (ps++[(RefProto r)]) )
		where r = subRHSProtoRef t
	(BodyProto b) -> ( t', (ps++[(BodyProto b')]) )
		where (t',b') = subBodyRef (t,(Body [])) b
	otherwise -> ( t, (ps++[p]) )

{--

{------------------------------------------------------------------------------
    find all references
------------------------------------------------------------------------------}

addPrefix :: Identifier -> Reference -> Reference
addPrefix id (Reference ids) = Reference(id:ids)

class HasRefList a where
	refList :: a -> [Reference]
	
instance HasRefList Assignment where
	-- if the assignment has a single identifier on the left, that is valid reference
	-- if the rhs is a block, then we can prefix the lhs to every value in the rhs
	refList (Assignment (Reference [id]) val) = (Reference [id]):(map (addPrefix id) (refList val))
	-- if the thing on the left is a reference, then we can't do anything (we need to sub it later)
	refList (Assignment (Reference (_:_)) _) = []
	
instance HasRefList Prototype where
	-- if the thing on the right is a reference, we can't do anythign (we need to sub it later)
	refList (RefProto ref) = []
	-- if it is a body, then return the protolist from the body
	refList (BodyProto b) = refList b
		
instance HasRefList Value where
	refList (BasicValue bv) = []
	refList (LinkValue ref) = []
	refList (ProtoValue ps) = nub $ concat $ map refList ps

instance HasRefList Body where
	refList (Body as) = nub $ concat $ map refList as

--}

{------------------------------------------------------------------------------
    compile tests with both compilers & compare the result
------------------------------------------------------------------------------}

type Compile = Opts -> String -> IO (Either Error String)

prop_CompareScala :: Opts -> Compile -> SfSource -> Property
prop_CompareScala opts compile (SfSource source) = not (null source) ==> monadicIO test where
	test = do
		isSame <- run $ compileForTest opts compile source
		assert $ isSame

compileForTest :: Opts -> Compile -> String -> IO (Bool)
compileForTest opts compile source = do

		let srcPath = tmpPath opts
		writeFile srcPath source
		compareWithScala opts compile srcPath

tmpPath :: Opts -> String
tmpPath opts = 
	if (isAbsolute outDir)
		then outDir </> "quickcheck.sf"
		else "/tmp" </> "quickcheck.sf"
	where outDir = outputPath opts

checkWithScala :: Opts -> Compile -> IO()
checkWithScala opts compile = do
	if (verbosity opts >= Debug)
		then verboseCheck (prop_CompareScala opts compile)
		else quickCheck (prop_CompareScala opts compile)

{------------------------------------------------------------------------------
    these not yet implemented
------------------------------------------------------------------------------}

checkWithOCaml :: Opts -> Compile -> IO()
checkWithOCaml opts compile = undefined

checkWithHP :: Opts -> Compile -> IO()
checkWithHP opts compile = undefined
