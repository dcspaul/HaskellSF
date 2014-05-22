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
import Data.String.Utils (endswith)
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

-- a reference appearing on the LHS of an assignment is
-- either a single identifier (foo) or a compound reference (a:b:foo)
-- we need to be able to generate sensible values for the references,
-- so that they point at valid entities. We can't do that yet because
-- the AST has not yet been created, so we simple insert placeholders
-- which we will populate later.
-- TODO: think about the frequencies later

arbitraryLHSRef :: Gen Reference
arbitraryLHSRef = frequency [(2,return dummyId),(1,return dummyRef)]
	where
		dummyRef = (Reference [Identifier "?ref"]) 
		dummyId = (Reference [Identifier "?id"]) 

-- a reference appearing on the RHS of an assignment must refer to something
-- we just generate a marker so we can substitute them with something valid later on

arbitraryRHSRef :: Gen Reference
arbitraryRHSRef = do
	return (Reference [Identifier "?ref"])

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

-- a value is a BasicValue or an (?rhs) Reference, or a list of Prototypes

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM BasicValue arbitrary
		, liftM LinkValue arbitraryRHSRef
		, do
			first <- arbitrary
			rest <- (resize 3) arbitrary
			return (ProtoValue (first:rest))
		]

-- assignment is an arbitrary (?lhs or ?id) Reference and an arbitrary Value

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitraryLHSRef arbitrary

-- prototype is an arbitrary Body, or an arbitrary (?rhs) Reference

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
		-- TODO: this forces sfConfig to be a block
		-- occasionally we might want to make it a value to test the error condition
		-- the following line makes it arbitrary (but that is abit too frequent)
		-- let a = Assignment (Reference [Identifier "sfConfig"]) sfConfig
		let a = Assignment (Reference [Identifier "sfConfig"]) (ProtoValue sfConfig)
		right <- ((resize 3) arbitrary)
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
	, values :: [Reference]		-- references of all known values
	}

-- the initial state
initialState = SubState
	{ path = Reference []
	, random = [1..] -- TODO: make this a list of random numbers? 
	, values = []
	}

-- start with an empty body
initialBody = (Body [])

-- assignment types
data VType = BlockType | ValueType | AnyType deriving(Eq)

-- random element of list
randomElt :: [a] -> Int -> a
randomElt es n = (es !! (n `mod` (length es)))

-- invent a LHS for an assignment
-- it is to be either an identifier 
-- or a reference to an item of the specified type
inventLHS :: SubState -> VType -> Reference -> (SubState,Reference)
inventLHS s t lhs = case lhs of			
	(Reference [Identifier "?id"]) -> inventID s t
	(Reference [Identifier "?ref"]) -> inventLHSRef s t
	_ -> undefined -- not possible
		
-- invent a reference for the LHS of an assignment
-- it should point to something of the specified type
-- if there isn't anything of the specified type,
-- then just invent an id
inventLHSRef :: SubState -> VType -> (SubState,Reference)
inventLHSRef s t = r where
	(n:ns) = random s
	vs = filter (hasType t) (values s)
	r = if (null vs) then (inventID s t)
		else ( s { random = ns }, randomElt vs n )
		
-- invent an identifier of the specified type
inventID :: SubState -> VType -> (SubState,Reference)
inventID s t = case t of
		BlockType -> inventID' "p"
		ValueType -> inventID' "v"
		_ -> undefined -- not possible
	where
		(n:ns) = random s
		is = map (:[]) ['A' .. 'Z']
		i = is !! (n `mod` (length is))
		inventID' sfx = ( s { random = ns }, (Reference [Identifier (i++sfx)]) )
	
-- invent a reference for the RHS of an assignment
-- it should point to something of the specified type
-- if there isn't anything of the specified type,
-- then invent a value
inventRHS :: SubState -> VType -> (SubState,Value)
inventRHS s t = r where
	(n:ns) = random s
	vs = filter (hasType t) (values s)
	r = if (null vs) then ( s, inventValue t )
		else ( s { random = ns }, LinkValue (randomElt vs n) )
		
-- invent a value for the RHS
-- either an empty block or a random literal, depending on the required type
inventValue :: VType -> Value
inventValue t = case t of
		BlockType -> ProtoValue [BodyProto (Body [])]
		ValueType -> BasicValue (StringValue "string")
		_ -> undefined -- not possible

-- does a reference have type "t" ?
-- we use the last letter of the name to denote the type
-- "v" is a value, "p" is a prototype (block)
hasType t (Reference r) = hasType' t r
hasType' _ [] = False
hasType' ValueType [Identifier "sfConfig"] = True
hasType' _ [Identifier "sfConfig"] = False
hasType' ValueType [Identifier i] = endswith "v" i
hasType' BlockType [Identifier i] = endswith "p" i
hasType' _ [Identifier i] = False
hasType' t (i:is) = hasType' t is
	
-- TODO: at some point, we may want to generate valid forward references so 
-- we can test ther HP semantics, or test for errors
-- one easy way of doing this would be to ramdomly swap the order
-- of the blocks once they have been generated ....
-- although, I guess this wouldn't generate (for example) loops

-- generate a configuration from a list of arbitrary assignments by
-- substituting the (?lhs,?rhs,?id) "placeholders" with arbitrary, valid values

subRefs :: [Assignment] -> SFConfig 
subRefs as = (SFConfig as')
	where ( _, (Body as') ) = subBodyRef ( initialState, initialBody ) (Body as)

-- substitute a list of assignments (left to right), propagating the state
subBodyRef :: (SubState,Body) -> Body -> (SubState,Body)
subBodyRef (s,body) (Body as) = foldl subAssignRef (s,body) as

-- substitute one assignment
subAssignRef :: (SubState,Body) -> Assignment -> (SubState,Body)
subAssignRef (s,body) (Assignment lhs rhs) = let

		-- the type of the rhs
		rhsType = case rhs of 
			LinkValue (Reference [Identifier "?ref"]) -> AnyType
			BasicValue _ -> ValueType
			ProtoValue _ -> BlockType
			LinkValue _ -> undefined -- not possible
			
		-- the type of the lhs
		lhsType = case lhs of
			(Reference [Identifier "?ref"]) -> AnyType
			(Reference [Identifier "?id"]) -> AnyType
			(Reference [Identifier "sfConfig"]) -> BlockType
			_ -> undefined -- not possible
	
		-- the type of the assignment
		(s1,t) = case lhsType of
				AnyType -> case rhsType of
					AnyType -> ( s { random=ns }, ( randomElt [ BlockType, ValueType ] n ) )
					otherwise -> (s,rhsType)
				otherwise -> (s,lhsType)
 			where (n:ns) = random s

		-- invent the LHS if we need to
		(s2,lhs2) = if (lhsType == AnyType) then (inventLHS s1 t lhs) else (s1,lhs)
	
		-- invent the RHS if we need to
		(s3,rhs3) = if (rhsType == AnyType) then (inventRHS s2 t) else (s2,rhs)
		
		-- if the rhs is a list of prototypes, substitute within the prototypes
		-- TODO: since we do this before we add the lhs to the symbol table,
		-- we avoid self-references. although we might want to do this occasionally
		-- as a test
		(s4,rhs4) = case rhs of
			(ProtoValue ps) -> subProtoListRef s3 lhs2 ps
			otherwise -> (s3,rhs3)

		-- add lhs to the symbol table
		(s5,lhs5) = addRef s4 lhs2
	
		-- remove the common prefix
		-- TODO: sometimes we should perhaps leave the full pathname (or some of it?)
		lhs6 = stripCommonPrefix (path s5) lhs5

	in ( s5, (appendToBody body (Assignment lhs6 rhs4)))
		where appendToBody (Body as) a = Body (as ++ [a])


-- remove the common prefix from reference
stripCommonPrefix :: Reference -> Reference -> Reference
stripCommonPrefix (Reference r) (Reference r') = Reference (scp r r') where
	scp _ [] = []
	scp _ [i'] = [i']
	scp (i:is) (i':is') = if (i==i') then (scp is is') else (i':is')
	scp _ is' = is'

{--

-- choose an arbitrary reference for the lhs
-- if we don't know any references, then just use an arbitrary local name
-- the "f" is a function to filter the reference list for things of the right type
-- (either value or reference, or either)
-- TODO: we might want to specify whether we would prefer a block or a value
-- TODO: occasionally we should output an invalid reference (that doesn't exist)
-- TODO: it would also be nice (but hard?) to test forward references (should be an error)?
subLHSRef :: SubState -> ([Reference] -> [Reference]) -> (Reference,SubState)
subLHSRef s f = ( r , s { random = ns } )
	where
		(n:ns) = random s
		vs = f (values s)
		r = if (null vs) then (randomElt rs n) else (randomElt vs n)
			where
				i = randomRef n
				rs = f [ makeValueRef i, makeProtoRef i ]

-- random reference
randomRef :: Int -> Reference		
randomRef n = Reference [Identifier i]
	where
		is = map (:[]) ['A' .. 'Z']
		i = is !! (n `mod` (length is))

-- random element of list
randomElt :: [a] -> Int -> a
randomElt es n = (es !! (n `mod` (length es)))

--}

-- add (relative) reference to the symbol table at the current path
-- return the (absolute) reference for the symbol and the new symbol table
-- don't add sfConfig to the symbol table
-- (just because it is confusing to have items randomly named sfConfig)
addRef :: SubState -> Reference -> (SubState,Reference)
addRef s (Reference is) = ( s', r )
	where
		(Reference ps) = path s
		r = Reference (ps++is)
		s' = if ((head is) == Identifier "sfConfig")
			then s else s { values = r:(values s) }

{--
-- choose a arbitrary reference for a link
-- if we don't have any valid references, then just output an empty block
-- TODO: occasionally we should output an invalid reference
-- TODO: or one which has the wrong type
subRHSLinkRef :: SubState -> (Value,SubState)
subRHSLinkRef s = ( v, s { random = ns } )
	where
		(n:ns) = random s
		vs = filter isProtoRef (values s)
		v = if (null vs) then emptyBlock else randomLink
			where
				emptyBlock = ProtoValue [BodyProto (Body [])]
				randomLink = LinkValue (stripCommonPrefix (path s) (randomElt vs n))
--}
			
-- substitute references in a list of prototypes (left to right), propagating the state
subProtoListRef :: SubState -> Reference -> [Prototype] -> (SubState,Value)
subProtoListRef s path ps = (s',(ProtoValue ps'))
	where (s',ps') = foldl (subProtoRef path) (s,[]) ps

-- substitute references in a prototype
subProtoRef :: Reference -> (SubState,[Prototype]) -> Prototype -> (SubState,[Prototype])
subProtoRef path' (s,ps) p = case p of
	
	-- if the prototype is a reference, substitute a random reference	
	(RefProto (Reference [Identifier "?ref"])) -> ( s', (ps++[r]) )
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
-- TODO: or one which has the wrong type
subRHSProtoRef :: SubState -> (Prototype,SubState)
subRHSProtoRef s = ( v, s { random = ns } )
	where
		(n:ns) = random s
		vs = filter (hasType BlockType) (values s)
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
