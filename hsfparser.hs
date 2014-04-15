{--
 ** SmartFrog parser
--}

import Data.Map (foldrWithKey)
import Data.List
import Data.List.Split (splitOn) 
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
--
import Data.Map (Map)
import qualified Data.Map as Map

{--
 ** utility functions
--}

tabString :: Int -> String
tabString n = foldl1 (++) (replicate n " ")

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = join new . splitOn old

indentBlockBy :: String -> String -> String
indentBlockBy ts text
	| length text == 0		= text
	| otherwise				= "\n" ++ ts ++ (replace "\n" ("\n" ++ ts) text) ++ "\n"

indentBlock :: String -> String
indentBlock = indentBlockBy (tabString 2)

maybePair :: (a,Maybe b) -> Maybe (a,b)
maybePair (a,Nothing) = Nothing
maybePair (a,Just b) = Just (a,b)

{--
 ** abstract syntax
--}

-- the deriving is necessary so we can use these as map keys
data Identifier = Identifier [Char] deriving(Eq,Ord)
data Reference = Reference [Identifier]
data Body = Body [Assignment]
data BasicValue = BoolValue Bool | NumValue Integer | StringValue [Char] | NullValue
                | DataRef [Identifier] | Vector [BasicValue] deriving(Eq)
data Value = BasicValue BasicValue | LinkValue Reference | ProtoValue [Prototype]
data Assignment = Assignment Reference Value
data Prototype = RefProto Reference | BodyProto Body

instance Show Identifier where
	show (Identifier id) = id
instance Show Reference where
	show (Reference ids) = (intercalate ":" (map show ids))
instance Show Body where
	show (Body as) = intercalate "\n" (map show as)
instance Show Assignment where
	show (Assignment ref val) = (show ref) ++ " " ++ (show val)
instance Show Prototype where
	show (RefProto ref) = show ref
	show (BodyProto body) = "{" ++ bodyContents ++ "}"
		where bodyContents = (indentBlock (show body))
instance Show Value where
	show (BasicValue bv) = (show bv) ++ ";"
	show (LinkValue ref) = (show ref) ++ ";"
	show (ProtoValue ps) = "extends " ++ (intercalate ", " (map show ps))
instance Show BasicValue where
	show (BoolValue True) = "true"
	show (BoolValue False) = "false"
	show (NumValue n) = show n
	show (StringValue str) = show str
	show (NullValue) =  "NULL"
	show (DataRef ids) = intercalate ":" (map show ids)
	show (Vector bvs) = "[" ++ (intercalate "," (map show bvs)) ++ "]"

{--
 ** lexer
--}

lexer = P.makeTokenParser emptyDef {
	reservedNames = ["true", "false","NULL","DATA","extends"],
	reservedOpNames = ["{","}","[","]"],
	commentStart = "/*",
	commentEnd = "*/",
	commentLine = "//"
	}

m_identifier = P.identifier lexer
m_reserved = P.reserved lexer
m_reservedOp = P.reservedOp lexer
m_whiteSpace = P.whiteSpace lexer
m_integer = P.integer lexer
m_stringLiteral = P.stringLiteral lexer
m_colon = P.colon lexer
m_semi = P.semi lexer
m_comma = P.comma lexer
m_commaSep = P.commaSep lexer
m_braces = P.braces lexer
m_brackets = P.brackets lexer

{--
 ** parser
--}

ident :: Parser Identifier
ident = do { i <- m_identifier ; return (Identifier i) }

-- R :: = I(:I)*
reference :: Parser Reference
reference = do { ref <- ident `sepBy1` m_colon; return (Reference ref) }

-- BV ::= Bool | Num | Str | DATA R | Null | Vector [BV]
basicValue :: Parser BasicValue
basicValue = do { s <- m_stringLiteral ; return (StringValue s) }
	<|> do { n <- m_integer ; return (NumValue n) }
	<|> do { m_reserved "true" ; return (BoolValue True) }
	<|> do { m_reserved "false" ; return (BoolValue False) }
	<|> do { m_reserved "NULL" ; return NullValue }
	<|> do { m_reserved "DATA" ; (Reference ref) <- reference ; return (DataRef ref) }
	<|> do { v <- m_brackets bvList ; return (Vector v) }
	where bvList = do { bvs <- m_commaSep basicValue; return (bvs) }

-- V ::= BV ; | LR ; | extends [PS]
value :: Parser Value
value = do { bv <- basicValue ; m_semi ; return (BasicValue bv) }
	<|> do { lr <- reference ; m_semi ; return (LinkValue lr) }
	<|> do { m_reserved "extends"; ps <- protoList; return (ProtoValue ps) }
	where protoList = do { ps <- prototype `sepBy1` m_comma; return (ps) }

-- A ::= R V
assignment :: Parser Assignment
assignment = do { lhs <- reference ; rhs <- value ; return (Assignment lhs rhs) }

-- B ::= [A]
body :: Parser Body
body = do { as <- assignment `sepBy` m_whiteSpace ; return (Body as) }

-- P ::= R | { B }
prototype :: Parser Prototype
prototype = do { ref <- reference ; return (RefProto ref) }
	<|> do { b <- m_braces body ; return (BodyProto b) }

-- SF ::= B <eof>
specification :: Parser Body
specification = do { m_whiteSpace; b <- body ; eof ; return b }

{--
 ** store
--}

data StoreValue = StoreValue BasicValue | SubStore Store deriving(Eq)
data Store = Store (Map Identifier StoreValue) deriving(Eq)

-- FIXME - display results in proper JSON
-- we need our own version of something to print the basic values 
-- so that we can format the JSON lists properly ...

instance Show Store where
	show (Store map) = indentBlock $ intercalate "\n" $ foldrWithKey showMapEntry [] map
		where showMapEntry k v result = ((show k) ++ ": " ++ (show v)):result
instance Show StoreValue where
	show (StoreValue bv) = (show bv)
	show (SubStore store) = indentBlock (show store)

-- I am using maps for the store which change the semantics 
-- slightly from Herry's implementation using lists
-- i.e. the order will be different 
-- I don't think this should be significant
-- if it is, you should be able to replace the store implementation
-- by changing these functions (and the data definition above)

lookupStore :: Identifier -> Store -> Maybe StoreValue
lookupStore id (Store map) = Map.lookup id map

putStore :: Identifier -> StoreValue -> Store -> Store
putStore id value (Store map) = (Store (Map.insert id value map))

emptyStore :: Store
emptyStore = (Store Map.empty)

{--
 ** semantic functions
--}

type NameSpace = Reference
type Context = (NameSpace,Store)
type ErrorMessage = String

sfPrefix :: Reference -> Reference
sfPrefix (Reference []) = (Reference [])
sfPrefix (Reference xs) = (Reference (init xs))

sfConcat :: Reference -> Reference -> Reference
sfConcat (Reference r1) (Reference r2) = (Reference (r1 ++ r2))

sfFind :: Store -> Reference -> Maybe StoreValue
sfFind store (Reference []) = Just (SubStore store)
sfFind store (Reference (id:ids)) = sfFind' ids (lookupStore id store)
	where
		sfFind' [] (Just v) = Just v
		sfFind' ids (Just (SubStore store)) = sfFind store (Reference ids)
		sfFind' _ _ = Nothing

sfResolv :: Context -> Reference -> Maybe (NameSpace,StoreValue)
sfResolv (Reference [], store) ref = maybePair (Reference [], sfFind store ref)
sfResolv (ns, store) ref
	| v == Nothing 		= sfResolv (sfPrefix ns, store) ns
	| otherwise 		= maybePair (ns, v)
	where v = sfFind store (sfConcat ns ref)

sfConfig :: Store -> Either ErrorMessage Store
sfConfig store =
	do {
		sfConfigValue <- case (sfFind store (Reference [Identifier "sfConfig"])) of
			Nothing -> Left "no sfConfig component"
			(Just value) -> Right value
		;
		sfConfigStore <- case sfConfigValue of
			(StoreValue v) -> Left ( "sfConfig component is basic value (" ++ (show v) ++ ")" )
			(SubStore store) -> Right store
		;
		return sfConfigStore
	}

sfRefLength :: Reference -> Int
sfRefLength (Reference ids) = length ids

{--
 ** evaluation functions
--}

type Evaluator a = Either ErrorMessage Context -> a -> Either ErrorMessage Context

initialContext :: Either ErrorMessage Context
initialContext = Right (Reference [],emptyStore)

evalBody :: Evaluator Body
evalBody context (Body assignments) = foldl evalAssignment context assignments

evalAssignment :: Evaluator Assignment
evalAssignment (Left errorMessage) _ = (Left errorMessage)
evalAssignment (Right (ns,store)) (Assignment ref v)
	| (sfRefLength ref) == 1		= evalValue (Right (ns,store)) (LinkValue (sfConcat ns ref))
	| otherwise = case (context) of
		  Nothing -> Left "hmm. can't resolv reference!"
		  Just (_, StoreValue _) -> Left "reference not an object (error 6)"
		  Just (ns', _) -> evalValue (Right (ns,store)) (LinkValue (sfConcat ns' ref))
		  where context = sfResolv (ns,store) (sfPrefix ref)
		  
evalValue :: Evaluator Value
evalValue context (BasicValue bv) = Left "eval BasicValue not implemented"
evalValue context (LinkValue ref) = Left "eval LinkValue not implemented"
evalValue context (ProtoValue [p]) = Left "eval ProtoValue not implemented"



-- evalValue :: Store -> Value -> Store
-- evalValue st s@(BasicValue bv) = (StoreValue s)
-- evalValue st (LinkValue ref) = (StoreValue (BasicValue NullValue))
-- evalValue st (ProtoValue ps) = (StoreValue (BasicValue NullValue))


-- *** main

evalSF :: (Either ParseError Body) -> Either ErrorMessage Store
evalSF (Left parseError) = Left ("parse error: " ++ (show parseError))
evalSF (Right parseTree) = do
	(_,store) <- evalBody initialContext parseTree
	result <- sfConfig store
	return result

compileSF :: String -> IO()
compileSF sourceFile = do
	parseResult <- parseFromFile specification sourceFile ;
	case (evalSF parseResult) of
		Left err  -> print ("SF compilation failed: " ++ err)
		Right store  -> print store

main = compileSF "/Users/paul/Work/Playground/HaskellSF/Test/patrick3.sf" 




