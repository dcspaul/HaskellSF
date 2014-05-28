{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

-- See: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
-- and: http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

module HSF.Test.QuickCheck
	( doCompare
	) where

import HSF.Options
import HSF.Parser
import HSF.Store
import HSF.Eval
import HSF.Errors
import HSF.Test.Frequencies
import HSF.Test.Invent
import HSF.Test.RunScalaVersion

import Data.List (intercalate,nub)
import Data.String.Utils (endswith)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert,monadicIO,run)
import Control.Monad (liftM,liftM2)
import System.FilePath.Posix (isAbsolute,(</>))

{------------------------------------------------------------------------------
    arbitrary items
------------------------------------------------------------------------------}

-- TODO: not yet supporting vectors ?

-- identifiers are arbitrary letters

instance Arbitrary Identifier where
	arbitrary =  oneof (map (return . Identifier) ids)
		where ids = map (:[]) ['a' .. 'z']

-- we need to have separate generators for references, depending on how
-- the references are used. so we don't define an Arbitrary instance -
-- use one of the following functions instead:

-- a reference appearing on the LHS of an assignment is
-- either a single identifier (foo) or a "placement" (a:b:foo)
-- we need to be able to generate sensible values for the placements,
-- so that they point at valid entities. We can't do that yet because
-- the AST has not yet been created, so we simply insert placeholders
-- (?ref) which we will populate later.

arbitraryLHSRef :: Gen Reference
arbitraryLHSRef = frequency [(iFreq,i),(rFreq,r)]
	where
		(iFreq,rFreq) = lhsFreq
		i = liftM Reference $ liftM (:[]) arbitrary
		r = return $ Reference [Identifier "?ref"]

-- a reference appearing on the RHS of an assignment must refer to something
-- we just generate a placeholder (?ref) so we can substitute them with something
-- valid later on

arbitraryRHSRef :: Gen Reference
arbitraryRHSRef = do
	return (Reference [Identifier "?ref"])

-- a body is a non-empty list of assignments, of the appropriate size

arbitraryBody :: Gen Body
arbitraryBody = sized arbitraryBody'

arbitraryBody' n 
	| n<=0 = return (Body [])
	| n>0 = do
		i <- sized $ \n -> choose(1,n)        -- choose the size of the first assignment (1..size)
		let ref = arbitraryLHSRef             -- no size restriction on the lhs
		let value = (resize i) arbitrary      -- the "size" of the value part
		(Body b') <- arbitraryBody' (n-i)	  -- the "size" of the remaining body
		a <- liftM2 Assignment ref value
		return (Body (a:b'))

-- a value is a BasicValue or an (?rhs) Reference, or a list of Prototypes

instance Arbitrary Value where
	arbitrary = frequency [(1,bv),(1,l),(1,p)]
		where
			bv = liftM BasicValue arbitrary
			l = liftM LinkValue arbitraryRHSRef
			p = liftM ProtoValue arbitraryProtoList

-- a list of prototypes

arbitraryProtoList = sized arbitraryProtoList'

arbitraryProtoList' n
	| n<=0 = return [BodyProto (Body [])]
	| n>0 = do
		i <- sized $ \n -> choose(1,n)     -- choose the size of the first prototype (1..size)
		p <- (resize i) arbitrary          -- the "size" of the first prototype
		ps <- arbitraryProtoList' (n-i)	   -- the "size" of the remaining list
		return (p:ps)

-- arbitrary basic values
-- data references are not evaluated, so we can just generate an arbitrary (non-empty)
-- list of Identifiers
-- we don't really care about the frequency of these
	
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

-- assignment is a (?ref or arbitrary id) and an arbitrary Value

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitraryLHSRef arbitrary

-- prototype is an arbitrary Body, or a (?ref)

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitraryBody
		, liftM RefProto arbitraryRHSRef
		]

-- the top-level body is slightly different, because ...
-- the aim here is to generate a list of assignments with at least one sfConfig
-- TODO: I guess you might want to test configurations with no sfconfig
-- so we could make this randomly something else ....
-- you might also want to test an empty top level?

arbitrarySFConfig = sized arbitrarySFConfig'
arbitrarySFConfig' n = do
		lsize <- sized $ \n -> choose(1,n)
		let rsize = (n-lsize)
		left <- ((resize lsize) arbitrary)
		first <- arbitrary
		rest <- (resize 10) arbitrary
		-- TODO: this forces sfConfig to be a block
		-- occasionally we might want to make it a value to test the error condition
		-- the following line makes it arbitrary (but that is abit too frequent)
		-- let a = Assignment (Reference [Identifier "sfConfig"]) (ProtoValue (first:rest))
		let a = Assignment (Reference [Identifier "sfConfig"]) (ProtoValue (first:rest))
		right <- ((resize rsize) arbitrary)
		-- we now invent plausible values for the references
		return (inventSF (SFConfig (left ++ [a] ++ right)))

{------------------------------------------------------------------------------
    top-level source
------------------------------------------------------------------------------}

newtype SfSource = SfSource String deriving(Eq)

instance Show SfSource where
	show (SfSource s) = id s
		
renderConfig :: SFConfig -> String
renderConfig = render

instance Arbitrary SfSource where
	arbitrary = liftM SfSource $ liftM renderConfig arbitrarySFConfig

{------------------------------------------------------------------------------
    compile tests with both compilers & compare the result
------------------------------------------------------------------------------}

type CompileFn = Opts -> String -> IO (Either Error String)
type CompareFn = Opts -> CompileFn -> String -> IO (Bool)

prop_Compare :: Opts -> CompileFn -> CompareFn -> SfSource -> Property
prop_Compare opts compile comp (SfSource source) = not (null source) ==> monadicIO test where
	test = do
		isSame <- run $ compileForTest opts compile comp source
		assert $ isSame

compileForTest :: Opts -> CompileFn -> CompareFn -> String -> IO (Bool)
compileForTest opts compile comp source = do

		let srcPath = tmpPath opts
		writeFile srcPath source
		comp opts compile srcPath

tmpPath :: Opts -> String
tmpPath opts =
	if (isAbsolute outDir)
		then outDir </> "quickcheck.sf"
		else "/tmp" </> "quickcheck.sf"
	where outDir = outputPath opts

doCompare :: Opts -> CompileFn -> CompareFn -> IO()
doCompare opts compile comp = do
	if (verbosity opts >= Debug)
		then verboseCheck (prop_Compare opts compile comp)
		else quickCheck (prop_Compare opts compile comp)
