{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.QuickCheck
	( check
	) where

import HSF.Options
import HSF.Parser
import HSF.Eval
import HSF.Compile
import HSF.RunScalaVersion

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad


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

-- TODO: not yet supporting vectors ?

instance Arbitrary Identifier where
	arbitrary = oneof
		[ liftM Identifier (return "a")
		, liftM Identifier (return "b")
		, liftM Identifier (return "c")
		, liftM Identifier (return "d")
		, liftM Identifier (return "e")
		, liftM Identifier (return "foo")
		]

-- TODO: this allows references to be empty

instance Arbitrary Reference where
	arbitrary = liftM Reference $ (resize 5) arbitrary
	
-- TODO: this allows bodies to be empty (maybe we want that sometimes?)

instance Arbitrary Body where
	arbitrary = liftM Body $ (resize 4) arbitrary

instance Arbitrary BasicValue where
	arbitrary = oneof
		[ liftM BoolValue arbitrary
		, liftM NumValue (return 1234)
		, liftM StringValue (return "string")
		, liftM DataRef $ (resize 5) arbitrary
		]

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM BasicValue arbitrary
		, liftM LinkValue arbitrary
		, liftM ProtoValue $ (resize 5) arbitrary
		]

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitrary arbitrary

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitrary
		, liftM RefProto arbitrary 
		]

newtype SfSource = SfSource String deriving(Eq)

instance Show SfSource where
	show (SfSource s) = id s

renderBody :: Body -> String
renderBody = render

instance Arbitrary SfSource where
	arbitrary = liftM SfSource $ liftM renderBody arbitrary

-- see: http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

prop_CompareScala :: Opts -> SfSource -> Property
prop_CompareScala opts (SfSource source) = not (null source) ==> monadicIO test where
	test = do
		isSame <- run $ compileForTest opts source
		assert $ isSame

compileForTest :: Opts -> String -> IO (Bool)
compileForTest opts source = do

		let srcPath = "/tmp/hsf-qc.sf"
		writeFile srcPath source
		haskellResult <- compile (opts { format=CompactJSON } ) srcPath
		scalaResult <- runSfParser opts srcPath

-- TODO: can we print the outputs *only* when the test fails ?

		putStrLn ( "Haskell: " ++ (resultString haskellResult))
		putStrLn ( "Scala: " ++ (resultString scalaResult) ++ "\n" )
		return (haskellResult == scalaResult)

tmpPath :: Opts -> String -> String
tmpPath opts srcPath = undefined
-- TODO: write this and use it
-- if the "output" option is absolute, use that as the location of the source
-- otherwise, use "/tmp" 

-- TODO: support a -v flag which we can use to control the printing level

check opts = do
	-- TODO: control these with the verbose option
	-- quickCheck prop_Foo
	verboseCheck (prop_CompareScala opts)





