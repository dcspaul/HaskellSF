{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

-- See: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
-- and: http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

module HSF.Test.QuickCheck
	( checkWithScala, checkWithOCaml, checkWithHP
	) where

import HSF.Options
import HSF.Parser
import HSF.Store
import HSF.Eval
import HSF.Errors
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

-- identifiers are arbitrary (uppercase) letters

instance Arbitrary Identifier where
	arbitrary =  oneof (map (return . Identifier) ids)
		where ids = map (:[]) ['A' .. 'Z']

-- we need to have separate generators for references, depending on how
-- the references are used. so we don't define an Arbitrary instance -
-- use one of the following functions instead:

-- a reference appearing on the LHS of an assignment is
-- either a single identifier (foo) or a compound reference (a:b:foo)
-- we need to be able to generate sensible values for the references,
-- so that they point at valid entities. We can't do that yet because
-- the AST has not yet been created, so we simply insert placeholders
-- (?ref) which we will populate later.
-- TODO: think about the frequencies later

arbitraryLHSRef :: Gen Reference
arbitraryLHSRef = frequency [(2,dummyId),(1, return dummyRef)]
	where
		dummyRef = (Reference [Identifier "?ref"]) 
		dummyId = liftM Reference $ liftM (:[]) arbitrary

-- a reference appearing on the RHS of an assignment must refer to something
-- we just generate a placeholder (?ref) so we can substitute them with something
-- valid later on

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

-- assignment is a (?ref or arbitrary id) and an arbitrary Value

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitraryLHSRef arbitrary

-- prototype is an arbitrary Body, or a (?ref)

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitrary
		, liftM RefProto arbitraryRHSRef
		]

-- the top-level body is slightly different, because ...
-- the aim here is to generate a list of assignments with at least one sfConfig
-- TODO: I guess you might want to test configurations with no sfconfig
-- so we could make this randomly something else ....
-- you might also want to test an empty top level?

instance Arbitrary SFConfig where
	arbitrary = do
		left <- ((resize 3) arbitrary)
		first <- arbitrary
		rest <- (resize 3) arbitrary
		-- TODO: this forces sfConfig to be a block
		-- occasionally we might want to make it a value to test the error condition
		-- the following line makes it arbitrary (but that is abit too frequent)
		-- let a = Assignment (Reference [Identifier "sfConfig"]) (ProtoValue (first:rest))
		let a = Assignment (Reference [Identifier "sfConfig"]) (ProtoValue (first:rest))
		right <- ((resize 3) arbitrary)
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
	arbitrary = liftM SfSource $ liftM renderConfig arbitrary

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
