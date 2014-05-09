{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.QuickCheck
	( check
	) where

import HSF.Utils
import HSF.Parser
import HSF.Eval

import Test.QuickCheck;
import Control.Monad;


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

instance Arbitrary BasicValue where
	arbitrary = oneof
		[ liftM BoolValue arbitrary
		, liftM NumValue (return 1234)
		, liftM StringValue (return "string")
		, liftM DataRef $ (resize 5) arbitrary
		]

instance Arbitrary Identifier where
	arbitrary = oneof
		[ liftM Identifier (return "a")
		, liftM Identifier (return "b")
		, liftM Identifier (return "c")
		, liftM Identifier (return "d")
		, liftM Identifier (return "e")
		, liftM Identifier (return "foo")
		]

instance Arbitrary Reference where
	arbitrary = liftM Reference $ (resize 5) arbitrary

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM BasicValue arbitrary
		, liftM ProtoValue $ (resize 5) arbitrary
		]

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitrary
		, liftM RefProto arbitrary 
		]

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitrary arbitrary

instance Arbitrary Body where
	arbitrary = liftM Body arbitrary

newtype SfSource = SfSource String deriving(Show,Eq)

renderBody :: Body -> String
renderBody = render

instance Arbitrary SfSource where
	arbitrary = liftM SfSource $ liftM renderBody arbitrary

-- this test currently returns True so that it keeps doing all the tests
-- eventually, we want to compile the source with the two compilers
-- and compare the text result
prop_Assign a = a /= (SfSource "some code here")

check = do
	-- quickCheck prop_Assign
	verboseCheck prop_Assign
