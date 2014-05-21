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
		return (subRefs (left ++ [a] ++ right))

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

-- the state of a variable substitution process
data SubState = SubState
	{ path :: Reference			-- the path to the current component
	, random :: [Int]			-- list of random numbers
	, blocks :: [Reference]		-- references of all known blocks - TODO: not currently used
	, values :: [Reference]		-- references of all known values - TODO: should distinguish blocks & values
	}

-- the initial state
initialState = SubState
	{ path = Reference []
	, random = [1..] -- TODO: make this a list of random numbers? 
	, blocks = []
	, values = []
	}

-- start with an empty body
initialBody = (Body [])

-- generate a configuration from a list of arbitrary assignments by
-- substituting the LHS and RHS "placeholders" with arbitrary, valid references
subRefs :: [Assignment] -> SFConfig 
subRefs as = (SFConfig as')
	where ( _, (Body as') ) = subBodyRef ( initialState, initialBody ) (Body as)

-- substitute a list of assignments (left to right), propagating the state
subBodyRef :: (SubState,Body) -> Body -> (SubState,Body)
subBodyRef (s,body) (Body as) = foldl subAssignRef (s,body) as

-- substitute one assignment
subAssignRef :: (SubState,Body) -> Assignment -> (SubState,Body)
subAssignRef (s,body) (Assignment r v) =
	let
		-- if the lhs is a placeholder, substitute it with a valid reference
		-- otherwise, add it to the symbol table
		-- r' is the absolute path of the reference on the lhs
		(r',s') = case r of
			(Reference [Identifier "LHS"]) -> subLHSRef s
			otherwise -> addRef s r
			
		-- if the rhs is a list of prototypes, substitute within the prototypes
		-- if it is a reference placeholder, substitute with a valid reference
		-- otherwise, use it unchanged	
		(v',s'') = case v of
			-- TODO: passing the state s here (rather than s')
			-- means that we never generate references to ourselves
			-- we should probaby do this occasionally as an error test
			(ProtoValue ps) -> subProtoListRef r' s ps
			(LinkValue (Reference [Identifier "RHS"])) -> subRHSLinkRef s
			otherwise -> (v,s')

		-- remove the common prefix
		-- TODO: sometimes we should perhaps leave the full pathname (or some of it?)
		r'' = stripCommonPrefix (path s) r'

	in ( s'', (appendToBody body (Assignment r'' v')))
		where appendToBody (Body as) a = Body (as ++ [a])

-- remove the common prefix from reference
stripCommonPrefix :: Reference -> Reference -> Reference
stripCommonPrefix (Reference r) (Reference r') = Reference (scp r r')
	where
		scp _ [] = [Identifier "???"]
		scp _ [i'] = [i']
		scp (i:is) (i':is') = if (i==i') then (scp is is') else (i':is')
		scp _ is' = is'

-- choose an arbitrary reference for the lhs
-- if we don't know any references, then just use an arbitrary local name
-- TODO: we might want to specify whether we would prefer a block or a value
-- TODO: occasionally we should output an invalid reference (that doesn't exist)
-- TODO: it would also be nice (but hard?) to test forward references (should be an error)?
subLHSRef :: SubState -> (Reference,SubState)
subLHSRef s = ( r , s { random = ns } )
	where
		(n:ns) = random s
		vs = (values s)
		r = if (null vs) then (randomRef n) else (randomElt vs n)

-- random reference
randomRef :: Int -> Reference		
randomRef n = Reference [Identifier i]
	where
		is = map (:[]) ['A' .. 'Z']
		i = is !! (n `mod` (length is))

-- random element of list
randomElt :: [a] -> Int -> a
randomElt es n = (es !! (n `mod` (length es)))

-- add (relative) reference to the symbol table at the current path
-- return the (absolute) reference for the symbol and the new symbol table
-- don't add sfConfig to the symbol table
-- (just because it is confusing to have items randomly named sfConfig)
addRef :: SubState -> Reference -> (Reference,SubState)
addRef s (Reference is) = ( r , s' )
	where
		(Reference ps) = path s
		r = Reference (ps++is)
		s' = if (is == [Identifier "sfConfig"])
			then s
			else s { values = r:(values s) }

-- choose a arbitrary reference for a link
-- if we don't have any valid references, then just output an empty block
-- TODO: occasionally we should output an invalid reference
-- TODO: this really needs to point at a prototype to be legal - can we do that most of the time?
subRHSLinkRef :: SubState -> (Value,SubState)
subRHSLinkRef s = ( v, s { random = ns } )
	where
		(n:ns) = random s
		vs = (values s)
		v = if (null vs) then emptyBlock else randomLink
			where
				emptyBlock = ProtoValue [BodyProto (Body [])]
				randomLink = LinkValue (stripCommonPrefix (path s) (randomElt vs n))
			
-- substitute references in a list of prototypes (left to right), propagating the state
subProtoListRef :: Reference -> SubState -> [Prototype] -> (Value,SubState)
subProtoListRef path s ps = ((ProtoValue ps'),s')
	where (s',ps') = foldl (subProtoRef path) (s,[]) ps

-- substitute references in a prototype
subProtoRef :: Reference -> (SubState,[Prototype]) -> Prototype -> (SubState,[Prototype])
subProtoRef path' (s,ps) p = case p of
	
	-- if the prototype is a reference, substitute a random reference	
	(RefProto (Reference [Identifier "RHS"])) -> ( s', (ps++[r]) )
		where (r,s') = subRHSProtoRef s

	-- if the prototype is a block, we recurse down
	-- notice that we set the new path before we descend
	-- and reset it when we return
	(BodyProto b) -> ( s3, (ps++[(BodyProto b')]) )
		where
			s1 = s { path = path' }
			(s2,b') = subBodyRef (s1,(Body [])) b
			s3 = s2 { path = (path s) }

-- choose a arbitrary reference for a link
-- if we don't have any valid references, then just output an empty block
-- TODO: occasionally we should output an invalid reference
-- TODO: this really needs to point at a prototype to be legal - can we do that most of the time?
subRHSProtoRef :: SubState -> (Prototype,SubState)
subRHSProtoRef s = ( v, s { random = ns } )
	where
		(n:ns) = random s
		vs = (values s)
		v = if (null vs) then emptyProto else randomProto
			where
				emptyProto = BodyProto (Body [])
				randomProto = RefProto (randomElt vs n)

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
