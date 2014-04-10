-- import Text.ParserCombinators.Parsec

import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

data Identifier = Identifier [Char]
data Reference = Reference [Identifier]
data LinkRef = LinkRef [Identifier]
data Body = Body [Assignment]
data BasicValue = BoolValue Bool | NumValue Integer | StringValue [Char] | NullValue
                | DataRef [Identifier] | Vector [BasicValue]
data Value = BasicValue BasicValue | LinkValue LinkRef | ProtoValue [Prototype]
data Assignment = Assignment Reference Value
data Prototype = RefProto Reference | BodyProto Body

instance Show Identifier where
	show (Identifier str) = str
instance Show Reference where
	show (Reference ids) = intercalate ":" (map show ids)
instance Show LinkRef where
	show (LinkRef ids) = intercalate ":" (map show ids)
instance Show Body where
	show (Body as) = intercalate " " (map show as)
instance Show Assignment where
	show (Assignment ref val) = (show ref) ++ " " ++ (show val)
instance Show Prototype where
	show (RefProto ref) = (show ref)
	show (BodyProto body) = "{" ++ (show body) ++ "}"
instance Show Value where
	show (BasicValue bv) = (show bv) ++ ";"
	show (LinkValue ref) = (show ref) ++ ";" 
	show (ProtoValue ps) = "extends " ++ (intercalate "," (map show ps))
instance Show BasicValue where
	show (BoolValue True) = "true"
	show (BoolValue False) = "false"
	show (NumValue n) = show n
	show (StringValue str) = show str
	show (NullValue) = "NULL"
	show (DataRef ids) = intercalate ":" (map show ids)
	show (Vector bvs) = "[" ++ (intercalate "," (map show bvs)) ++ "]"

-- **** the lexer

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

-- **** the parser

ident :: Parser Identifier
ident = do { i <- m_identifier ; return (Identifier i) }

-- R :: = I(:I)*
reference :: Parser Reference
reference = do { ref <- ident `sepBy1` m_colon ; return (Reference ref) }

-- LR ::= R
linkref :: Parser LinkRef
linkref = do { (Reference ref) <- reference ; return (LinkRef ref) }

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
	<|> do { lr <- linkref ; m_semi ; return (LinkValue lr) }
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

-- *** main

main = do { result <- parseFromFile specification "/Users/paul/Work/Playground/HaskellSF/Test/features.sf"
	; case (result) of
		Left err  -> print err
		Right strings  -> print strings
		}
