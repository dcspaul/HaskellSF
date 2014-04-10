-- import Text.ParserCombinators.Parsec

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

data Identifier = Identifier [Char] deriving(Show)
data Reference = Reference [Identifier] deriving(Show)
data LinkRef = LinkRef [Identifier] deriving(Show)
data Body = Body [Assignment] deriving(Show)
data BasicValue = BoolValue Bool | NumValue Integer | StringValue [Char] | NullValue
                | DataRef [Identifier] | Vector [BasicValue] deriving(Show)
data Value = BasicValue BasicValue | LinkValue LinkRef | ProtoValue [Prototype] deriving(Show)
data Assignment = Assignment Reference Value deriving(Show)
data Prototype = RefProto Reference | BodyProto Body deriving(Show)

-- **** the lexer

lexer = P.makeTokenParser emptyDef {
	reservedNames = ["true", "false","NULL","DATA","extends"],
	reservedOpNames = ["{","}","[","]"]
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

-- **** the parser

ident :: Parser Identifier
ident = do { ident <- m_identifier ; return (Identifier ident) }

-- R :: = I(:I)*
reference :: Parser Reference
reference = do { id_list <- ident `sepBy1` m_colon ; return (Reference id_list) }

-- LR ::= R
linkref :: Parser LinkRef
linkref = do { (Reference ref) <- reference ; return (LinkRef ref) }

-- BV ::= Bool | Num | Str | DATA R | Null | Vector [BV]
basicValue :: Parser BasicValue
basicValue = do { num <- m_stringLiteral ; return (StringValue num) }
	<|> do { num <- m_integer ; return (NumValue num) }
	<|> do { m_reserved "true" ; return (BoolValue True) }
	<|> do { m_reserved "false" ; return (BoolValue False) }
	<|> do { m_reserved "NULL" ; return NullValue }
	<|> do { m_reserved "DATA" ; (Reference ref) <- reference ; return (DataRef ref) }
	<|> do { m_reservedOp "[" ; v <- m_commaSep basicValue; m_reservedOp "]" ; return (Vector v) }

-- V ::= BV ; | LR ; | extends [PS]
value :: Parser Value
value = do { bv <- basicValue ; m_semi ; return (BasicValue bv) }
	<|> do { lr <- linkref ; m_semi ; return (LinkValue lr) }
	<|> do { m_reserved "extends"; elt_list <- prototype `sepBy1` m_comma ; return (ProtoValue elt_list) }

-- A ::= R V
assignment :: Parser Assignment
assignment = do { lhs <- reference ; rhs <- value ; return (Assignment lhs rhs) }

-- B ::= [A]
body :: Parser Body
body = do { assignment_list <- assignment `sepBy` m_whiteSpace ; return (Body assignment_list) }

-- P ::= R | { B }
prototype :: Parser Prototype
prototype = do { ref <- reference ; return (RefProto ref) }
	<|> do { try (m_reservedOp "{") ; b <- body ; try (m_reservedOp "}") ; return (BodyProto b) }

-- SF ::= B <eof>
specification :: Parser Body
specification = do { b <- body ; eof ; return b }

-- *** main

main = do { result <- parseFromFile specification "/Users/paul/Work/Playground/HaskellSF/Test/herry4.sf"
	; case (result) of
		Left err  -> print err
		Right strings  -> print strings
		}
