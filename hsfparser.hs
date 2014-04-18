{------------------------------------------------------------------------------
    SmartFrog evaluator
------------------------------------------------------------------------------}

import Data.Map (foldrWithKey)
import Data.List
import Data.List.Split (splitOn) 
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

{------------------------------------------------------------------------------
    utility functions
------------------------------------------------------------------------------}

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

-- there must be a nice generic way of saying this ?
maybePair :: (a,Maybe b) -> Maybe (a,b)
maybePair (a,Nothing) = Nothing
maybePair (a,Just b) = Just (a,b)

{------------------------------------------------------------------------------
    abstract syntax
------------------------------------------------------------------------------}

data Identifier = Identifier [Char] deriving(Eq)
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

{------------------------------------------------------------------------------
    lexer
------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------
    parser
------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------
    store
------------------------------------------------------------------------------}

data StoreValue = StoreValue BasicValue | SubStore Store deriving(Eq,Show)
data Store = Store [(Identifier,StoreValue)] deriving(Eq,Show)

prefixToStore ( i, v, Store s ) = Store ((i,v):s)

isSubStore :: StoreValue -> Bool
isSubStore (SubStore _) = True
isSubStore (StoreValue _) = False

class StoreItem a where
	showItem :: a -> String 
	
instance StoreItem Identifier where
	showItem (Identifier i) = id i
instance StoreItem Store where
	showItem (Store as) =
		(intercalate ",\n" (map showItemEntry as))
		where
			showItemEntry (i, StoreValue bv) = (showItem i) ++ ": " ++ (showItem bv)
			showItemEntry (i, SubStore s) = (showItem i) ++ ": {" ++ (indentBlock (showItem s))  ++ "}"
instance StoreItem BasicValue where
	showItem (BoolValue True) = "true"
	showItem (BoolValue False) = "false"
	showItem (NumValue n) = show n
	showItem (StringValue str) = id str
	showItem (NullValue) = "Null"
	showItem (DataRef ids) = intercalate ":" (map showItem ids)
	showItem (Vector bvs) = "[\n" ++ (indentBlock (intercalate ",\n" (map showItem bvs)))  ++ "\n]"

{------------------------------------------------------------------------------
    semantic functions
------------------------------------------------------------------------------}

type NameSpace = Reference
type ErrorMessage = String
type Result = Either ErrorMessage Store

-- 6.7
(|+|) :: Reference -> Reference -> Reference
(|+|) (Reference r1) (Reference r2) = (Reference (r1 ++ r2))

-- 6.11
sfPrefix :: Reference -> Reference
sfPrefix (Reference []) = (Reference [])
sfPrefix (Reference [_]) = (Reference [])
sfPrefix (Reference is) = (Reference (init is))

-- 6.12
sfPut :: (Store,Identifier,StoreValue) -> Store
sfPut ( Store [], i, v ) = Store [(i,v)]
sfPut ( Store ((is,vs):s'), i, v )
	| is == i	= Store ((i,v):s')
	| otherwise = prefixToStore(is,vs,sfPut(Store s',i,v))

-- 6.13
sfBind :: (Store,Reference,StoreValue) -> Result

sfBind ( _, Reference [], _ ) = Left "error 3 (attempt to replace root store)"

sfBind( s, Reference [i], v ) = Right (sfPut (s,i,v))

sfBind( Store [], Reference (i:r'), v ) = Left "error 2 (reference has no parent)"

sfBind( Store ((is,vs@(SubStore ss)):s'), Reference (i:r'), v )
	| is == i		= do { s'' <- sfBind(ss,Reference r',v) ; return (Store ((i,(SubStore s'')):s')) }
	| otherwise		= do { s'' <- sfBind(Store s',Reference (i:r'),v) ; return (prefixToStore (is,vs,s''))  }

sfBind( Store ((is,vs@(StoreValue sv)):s'), Reference (i:r'), v )
	| is == i		= Left "error 1 (parent not a store)"
	| otherwise		= do { s'' <- sfBind(Store s',Reference (i:r'),v) ; return (prefixToStore (is,vs,s''))  }

-- 6.14 
sfFind :: (Store,Reference) -> Maybe StoreValue

sfFind (s, (Reference [])) = Just (SubStore s)

sfFind ((Store []), _) = Nothing

sfFind ((Store ((is,vs):s')), Reference [i])
	| is == i   = Just vs
	| otherwise = sfFind (Store s', Reference [i])

sfFind ((Store ((is,vs@(SubStore ss)):s')), Reference (i:r'))
	| is == i		= sfFind(ss,Reference r')
	| otherwise 	= sfFind (Store s', Reference (i:r'))

sfFind ((Store ((is,vs@(StoreValue sv)):s')), Reference (i:r'))
	| is == i		= Nothing
	| otherwise 	= sfFind (Store s', Reference (i:r'))

-- 6.16
sfResolv :: (Store,NameSpace,Reference) -> Maybe (NameSpace,StoreValue)

sfResolv (s, Reference [], r) = maybePair (Reference [], sfFind(s,r))

sfResolv (s, ns, r)
	| v == Nothing 		= sfResolv (s, sfPrefix ns, r)
	| otherwise 		= maybePair (ns, v)
	where v = sfFind (s, ns |+| r)

-- 6.17
sfCopy :: (Store,Store,Reference) -> Result

sfCopy ( s1, Store [], pfx ) = Right s1

sfCopy ( s1, Store ((i,v):s2), pfx ) = do
	s' <- sfBind( s1, pfx |+| (Reference [i]), v)
	sfCopy (s',Store s2,pfx)

-- 6.18
sfInherit :: (Store,NameSpace,Reference,Reference) -> Result

sfInherit (s, ns, p, r) =
	case (sfResolv(s,ns,p)) of
		Nothing -> Left ( "error 4 (can't resolve prototype): " ++ (show p) )
		Just (ns',SubStore s') -> sfCopy(s,s',r)
		Just (ns',StoreValue v') -> Left ( "error 4 (prototype is not a store): " ++ (show p) )

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

-- 6.22
evalBasicValue :: BasicValue -> Result
evalBasicValue _ = Left "evalBasicValue not implemented"

-- 6.23
evalProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> Result

evalProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	fB <- evalBody bp $ (r,s)
	fP <- evalProtoList ps $ (ns, r, fB)
	return fP

evalProtoList ((RefProto rp):ps) = \(ns,r,s) -> do
	s' <- sfInherit(s,ns,rp,r)
	fP <- evalProtoList ps $ (ns, r, s')
	return fP

evalProtoList ([]) = \(ns,r,s) -> (Right s)

-- 6.24
evalValue :: Value -> (NameSpace,Reference,Store) -> Result

evalValue (BasicValue bv) = \(ns,r,s) -> sfBind(s, r, StoreValue bv)

evalValue (LinkValue lr) = \(ns,r,s) -> do
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> Left ( "error 5 (can't resolve link value): " ++ (show lr) )
		Just (n,v) -> Right (n,v)
	s' <- sfBind(s, r, v')
	return s' 

evalValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	fP <- evalProtoList ps $ (ns,r,s')
	return fP
	
-- 6.25
evalAssignment :: Assignment -> (NameSpace,Store) -> Result

evalAssignment (Assignment r@(Reference [_]) v) = \(ns,s) -> do
	fV <- evalValue v $ (ns, (ns |+| r), s)
	return fV

evalAssignment (Assignment r v) = \(ns,s) -> do
	fV <- case (sfResolv (s,ns,(sfPrefix r))) of
		Nothing -> Left ( "error 6 (can't resolve reference): " ++ (show r) )
		Just (_, StoreValue _) -> Left ( "error 6 (reference not an object): " ++ (show r) )
		Just (ns', _) -> evalValue v $ (ns, ns' |+| r, s)
	return fV

-- 6.26
evalBody :: Body -> (NameSpace,Store) -> Result

evalBody (Body (a:b)) = \(ns,s) -> do
	fA <- evalAssignment a $ (ns,s)
	fB <- evalBody (Body b) $ (ns,fA)
	return fB

evalBody (Body []) = \(ns,s) -> (Right s)

-- 6.27
evalSpecification :: Body -> Result

evalSpecification b = do
	fB <- evalBody b $ (Reference [], Store [])
	case (sfFind(fB,Reference [Identifier "sfConfig"])) of
		Nothing -> Left "no sfConfig at top level of specification"
		Just (StoreValue _) -> Left "sfConfig cannot be a basic value"
		Just (SubStore s) -> return s

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

compileSF :: String -> IO()
compileSF sourceFile = do
	parseResult <- parseFromFile specification sourceFile ;
	case (parseResult) of
		Left err  -> print ("SF parser failed: " ++ (show err))
		Right body -> case (evalSpecification body) of
			Left errorMessage -> print $ errorMessage ++ "\n" ++ show body
			Right store -> putStr $ showItem store

main = compileSF "/Users/paul/Work/Playground/HaskellSF/Test/herry4.sf" 
