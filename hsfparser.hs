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

-- how much at neginning = leading l
-- hoe much to pass on to children = t

tabsize = 3
spaceit t str = (concat $ replicate t " ") ++ str

instance Show Identifier where show i = showIdentifier 0 0 i
showIdentifier l t (Identifier str) = spaceit l str

instance Show Reference where show r = showReference 0 0 r
showReference l t (Reference ids) = spaceit l (intercalate ":" (map (showIdentifier 0 t) ids))
	
instance Show LinkRef where show r = showLinkRef 0 0 r
showLinkRef l t (LinkRef ids) = spaceit l (intercalate ":" (map (showIdentifier 0 t) ids))
	
instance Show Body where show b = showBody 0 0 b
showBody l t (Body as) = intercalate "" (map (showAssignment l t) as)
	
instance Show Assignment where show a = showAssignment 0 0 a
showAssignment l t (Assignment ref val) = spaceit l (((showReference 0 t) ref) ++ " " ++ ((showValue 0 t) val))
	
instance Show Prototype where show p = showPrototype 0 0 p
showPrototype l t (RefProto ref) = spaceit l (((showReference 0 t) ref))
-- showPrototype l t (BodyProto (Body [])) = (spaceit l "{") ++ (showBody (t+tabsize) (t+tabsize) body) ++ (spaceit t "}")
showPrototype l t (BodyProto body) = (spaceit l "{\n") ++ (showBody (t+tabsize) (t+tabsize) body) ++ (spaceit t "}")
	
instance Show Value where show v = showValue 0 0 v
showValue l t (BasicValue bv) = spaceit l (((showBV 0 t) bv) ++ ";\n")
showValue l t (LinkValue ref) = spaceit l (((showLinkRef 0 t) ref) ++ ";\n")
showValue l t (ProtoValue ps) = spaceit l ("extends " ++ (intercalate ", " (map (showPrototype 0 t) ps)) ++ "\n")
	
instance Show BasicValue where show bv = showBV 0 0 bv
showBV l t (BoolValue True) = spaceit l ("true")
showBV l t (BoolValue False) = spaceit l ("false")
showBV l t (NumValue n) = spaceit l (show n)
showBV l t (StringValue str) = spaceit l (show str)
showBV l t (NullValue) = spaceit l ("NULL")
showBV l t (DataRef ids) = spaceit l (intercalate ":" (map (showIdentifier 0 t) ids))
showBV l t (Vector bvs) = spaceit l ("[" ++ (intercalate "," (map (showBV 0 t) bvs)) ++ "]")

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

main = do { result <- parseFromFile specification "/Users/paul/Work/Playground/HaskellSF/Test/herry4.sf"
	; case (result) of
		Left err  -> print err
		Right strings  -> print strings
		}
