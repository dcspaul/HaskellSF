{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: QuickCheck tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.QuickCheck
	( check
	) where

import HSF.Options
import HSF.Parser
import HSF.Eval
import HSF.Compile
import HSF.Test.RunScalaVersion

import Data.List (intercalate,nub)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad
import System.FilePath.Posix (isAbsolute,(</>))

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

-- TODO: for now, we generate only single-identifier references
-- we need a second pass to replace them with something chosen
-- from the list of "real" identifiers

instance Arbitrary Reference where
	arbitrary = do
		first <- arbitrary
		-- rest <- (resize 3) arbitrary
		return (Reference [first]) -- [Identifier]
		-- return (Reference (first:rest))

instance Arbitrary Body where
	arbitrary = sized body' where
		body' n
			| n<=1 = liftM Body ((resize 1) arbitrary)
			| n>1  = liftM Body ((resize n) arbitrary)

-- datarefs must not be empty

instance Arbitrary BasicValue where
	arbitrary = oneof
		[ liftM BoolValue arbitrary
		, liftM NumValue (return 1234)
		, liftM StringValue (return "string")
		, do
			first <- arbitrary
			rest <- (resize 3) arbitrary
			return (DataRef (first:rest)) -- [Identifier]
		]

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM BasicValue arbitrary -- BasicValue
		, liftM LinkValue arbitrary -- Reference
		, do
			first <- arbitrary
			rest <- (resize 3) arbitrary
			return (ProtoValue (first:rest)) -- [Prototype]
		]

instance Arbitrary Assignment where
	arbitrary = liftM2 Assignment arbitrary arbitrary -- Reference Value

instance Arbitrary Prototype where
	arbitrary = oneof
		[ liftM BodyProto arbitrary -- Body
		, liftM RefProto arbitrary -- Reference
		]

newtype SfSource = SfSource String deriving(Eq)

instance Show SfSource where
	show (SfSource s) = id s

data SFConfig = SFConfig [Assignment] deriving(Eq,Show)

-- the aim here is to generate a list of assignments with at least one sfConfig
-- TODO: I guess you might want to test configurations with no sfconfig
-- so we could make this randomly something else ....
-- you might also want to test an empty top level?

instance Arbitrary SFConfig where
	arbitrary = do
		left <- ((resize 3) arbitrary)
		sfConfig <- (resize 3) arbitrary
		let a = Assignment (Reference [Identifier "sfConfig"]) sfConfig
		right <- ((resize 3) arbitrary)
		return (SFConfig (left ++ [a] ++ right))

renderConfig :: SFConfig -> String
renderConfig = render

instance Arbitrary SfSource where
	arbitrary = liftM SfSource $ liftM renderConfig arbitrary

instance ParseItem SFConfig where
	render (SFConfig as) = intercalate "\n" (map render as)

-- see: http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

prop_CompareScala :: Opts -> SfSource -> Property
prop_CompareScala opts (SfSource source) = not (null source) ==> monadicIO test where
	test = do
		isSame <- run $ compileForTest opts source
		assert $ isSame

compileForTest :: Opts -> String -> IO (Bool)
compileForTest opts source = do

		let srcPath = tmpPath opts
		writeFile srcPath source
		haskellResult <- compile (opts { format=CompactJSON } ) srcPath
		scalaResult <- runSfParser opts srcPath
		if (not $ matchSfParser haskellResult scalaResult) then do
			putStrLn ( "Haskell: " ++ (resultString haskellResult) )
			putStrLn ( "Scala:   " ++ (resultString scalaResult) )
			return False
		else return True

tmpPath :: Opts -> String
tmpPath opts = 
	if (isAbsolute outDir)
		then outDir </> "quickcheck.sf"
		else "/tmp" </> "quickcheck.sf"
	where outDir = outputPath opts
		
-- TODO: support a -v flag which we can use to control the printing level

check opts = do
	-- TODO: control these with the verbose option
	-- quickCheck prop_Foo
	verboseCheck (prop_CompareScala opts)



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
