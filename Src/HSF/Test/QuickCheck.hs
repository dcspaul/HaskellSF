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
		-- TODO: sfConfig should be a block most of the time
		-- (rather than a simple value)
		-- otherwise, you just get lots of errors
		sfConfig <- (resize 3) arbitrary
		let a = Assignment (Reference [Identifier "sfConfig"]) sfConfig
		right <- ((resize 3) arbitrary)
		-- TODO: make the list of numbers random ...
		-- see: http://www.haskell.org/haskellwiki/Random_list
		return (evalRefs [1..] (left ++ [a] ++ right))

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

-- TODO: this is not really just the symbol table
-- the first thing is the current "path"
-- the second thing is the list of random numbers
-- the third thing is the list of known references
-- so it is really a more general type of "state"
type SymTab = (Reference,[Int],[Reference])

-- process a list of assignments by
-- replacing the LHS and RHS "placeholders" with arbitrary, valid references
evalRefs :: [Int] -> [Assignment] -> SFConfig 
evalRefs ns as = (SFConfig as')
	-- we start with an empty path, the suuplied list of random numbers, and an empty body
	where (_,(Body as')) = subBodyRef (((Reference []),ns,[]),(Body [])) (Body as)

-- evaluate list of assignments (left to right) 
subBodyRef :: (SymTab,Body) -> Body -> (SymTab,Body)
subBodyRef (t,body) (Body as) = foldl subAssignRef (t,body) as

-- evaluate one assignment
subAssignRef :: (SymTab,Body) -> Assignment -> (SymTab,Body)
subAssignRef (t,body) (Assignment r v) =
	let
		(r',t') = case r of
			(Reference [Identifier "LHS"]) -> subLHSRef t
			(Reference [Identifier i]) -> addRef t i
			otherwise -> (r,t)
		(v',t'') = case v of
			-- TODO: passing the table t here (rather than t')
			-- means that we don;t generate references to ourselves
			-- so we should probaby only do this occasionally
			(ProtoValue ps) -> subProtoListRef r' t ps
			(LinkValue (Reference [Identifier "RHS"])) -> subRHSLinkRef t'
			otherwise -> (v,t')
	in ( t'', (appendToBody body (Assignment r' v')))
		where appendToBody (Body as) a = Body (as ++ [a])

-- add s as an identifier to the symbol table at the current path
-- return the (absolute) reference for the symbol
-- and the new symbol table
addRef :: SymTab -> String -> (Reference,SymTab)
addRef ((Reference p),ns,t) s = ( r , ((Reference p),ns,t') )
	where
		r = Reference (p++[Identifier s])
		-- TODO: we don't add sfConfig to the symbol table
		-- maybe we should do this **occasionally**
		t' = if (s == "sfConfig") then t else (r:t)

-- TODO: all references are currently "absolute"
-- we can (randomly) make them relative by removing (some of) any common prefix

-- choose a reference for the lhs
-- if we don't have any references, use a single character value
-- use upper case letters so we can easily tell when this has happened
-- TODO: occasionally we should output an invalid reference
subLHSRef :: SymTab -> (Reference,SymTab)
subLHSRef (p,n:ns,[]) = ( Reference [Identifier i] , (p,ns,[]) )
	where
		is = map (:[]) ['A' .. 'Z']
		i = is !! (n `mod` (length is))
subLHSRef (p,n:ns,t) = ( r , (p,ns,t) )
	where r = t !! (n `mod` (length t))

-- choose a reference for a prototype
-- if we don't have any valid references, then just output an empty block
-- TODO: occasionally we should output an invalid reference
-- TODO: this really needs to point at a prototype to be legal - can we do that most of the time?
subRHSProtoRef :: SymTab -> (Prototype,SymTab)
subRHSProtoRef (p,ns,[]) = ( BodyProto (Body []) , (p,ns,[]) )
subRHSProtoRef (p,n:ns,t) = ( RefProto r , (p,ns,t) )
	where r = t !! (n `mod` (length t))

-- choose a reference for a link
-- if we don't have any valid references, then just output an empty block
-- TODO: occasionally we should output an invalid reference
-- TODO: this really needs to point at a prototype to be legal - can we do that most of the time?
subRHSLinkRef :: SymTab -> (Value,SymTab)
subRHSLinkRef (p,ns,[]) = ( ProtoValue [BodyProto (Body [])] , (p,ns,[]) )
subRHSLinkRef (p,n:ns,t) = ( LinkValue r , (p,ns,t) )
	where r = t !! (n `mod` (length t))

subProtoListRef :: Reference -> SymTab -> [Prototype] -> (Value,SymTab)
subProtoListRef r t ps = ((ProtoValue ps'),t')
	where (t',ps') = foldl (subProtoRef r) (t,[]) ps

subProtoRef :: Reference -> (SymTab,[Prototype]) -> Prototype -> (SymTab,[Prototype])
subProtoRef s (t,ps) p = case p of
	(RefProto (Reference [Identifier "RHS"])) -> ( t', (ps++[r]) )
		where (r,t') = subRHSProtoRef t
	-- notice the we use t' not t - to keep all the symbols from the deeper levels
	(BodyProto b) -> ( t', (ps++[(BodyProto b')]) )
		where
			-- add the prefix to the symbols before we descend into the block
			((Reference p),rs,ss) = t
			(Reference s') = s
			p' = Reference (p++s')
			-- process the block
			((_,rs',ss'),b') = subBodyRef ((p',rs,ss),(Body [])) b
			-- return to the old prefix
			t' = ((Reference p),rs',ss')
	otherwise -> ( t, (ps++[p]) )

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
