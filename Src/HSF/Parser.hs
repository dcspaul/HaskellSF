{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Parser
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Parser
	( Identifier(..), Reference(..) , Body(..) , BasicValue(..)
	, Value(..), Assignment(..), Prototype(..)
	, ParserIO
	, parseSF
	, ParseItem, render
	) where

import HSF.Utils

import Data.List (intercalate)
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix (combine, takeDirectory)
import Control.Exception (try,IOException)

-- cabal install Parsec
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Language (GenLanguageDef)
import Text.ParserCombinators.Parsec.Pos (newPos)
import qualified Text.Parsec.Token as P

{------------------------------------------------------------------------------
    abstract syntax
------------------------------------------------------------------------------}

data Identifier = Identifier [Char] deriving(Eq,Show)
data Reference = Reference [Identifier] deriving(Eq,Show)
data Body = Body [Assignment] deriving(Eq,Show)
data BasicValue = BoolValue Bool | NumValue Integer | StringValue [Char] | NullValue
                | DataRef [Identifier] | Vector [BasicValue] deriving(Eq,Show)
data Value = BasicValue BasicValue | LinkValue Reference | ProtoValue [Prototype] deriving(Eq,Show)
data Assignment = Assignment Reference Value deriving(Eq,Show)
data Prototype = RefProto Reference | BodyProto Body deriving(Eq,Show)

{------------------------------------------------------------------------------
    lexer
------------------------------------------------------------------------------}

-- normally, we would base our definition on "emptyDef"
-- but that doesn't seem to be possible with ParsecT (?)

type SfLanguageDef st = GenLanguageDef String st IO

sfDef :: SfLanguageDef st
sfDef = P.LanguageDef
	{ P.commentStart   = "/*"
	, P.commentEnd     = "*/"
	, P.commentLine    = "//"
	, P.nestedComments = True
	, P.identStart     = letter <|> char '_'
	, P.identLetter    = alphaNum <|> oneOf "_'"
	, P.opStart        = P.opLetter sfDef
	, P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
	, P.reservedOpNames= ["{","}","[","]"]
	, P.reservedNames  = ["true", "false","NULL","DATA","extends"]
	, P.caseSensitive  = True
	}

lexer = P.makeTokenParser sfDef

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
m_symbol = P.symbol lexer

{------------------------------------------------------------------------------
    parser
------------------------------------------------------------------------------}

-- the parser uses the IO monad with ParsecT
-- this is necessary to support the #include

type ParserIO t = ParsecT [Char] ParserState IO t

ident :: ParserIO Identifier
ident = do { i <- m_identifier ; return (Identifier i) }

-- R :: = I(:I)*
reference :: ParserIO Reference
reference = do { ref <- ident `sepBy1` m_colon; return (Reference ref) }

-- BV ::= Bool | Num | Str | DATA R | Null | Vector [BV]
basicValue :: ParserIO BasicValue
basicValue = do { s <- m_stringLiteral ; return (StringValue s) }
	<|> do { n <- m_integer ; return (NumValue n) }
	<|> do { m_reserved "true" ; return (BoolValue True) }
	<|> do { m_reserved "false" ; return (BoolValue False) }
	<|> do { m_reserved "NULL" ; return NullValue }
	<|> do { m_reserved "DATA" ; (Reference ref) <- reference ; return (DataRef ref) }
	<|> do { v <- m_brackets bvList ; return (Vector v) }
	where bvList = do { bvs <- m_commaSep basicValue; return (bvs) }

-- V ::= BV ; | LR ; | extends [PS]
value :: ParserIO Value
value = do { bv <- basicValue ; m_semi ; return (BasicValue bv) }
	<|> do { lr <- reference ; m_semi ; return (LinkValue lr) }
	<|> do { m_reserved "extends"; ps <- protoList; return (ProtoValue ps) }
	where protoList = do { ps <- prototype `sepBy1` m_comma; return (ps) }

-- A ::= R V
assignment :: ParserIO Assignment
assignment = do { lhs <- reference ; rhs <- value ; return (Assignment lhs rhs) }

-- these "statements" are not part of the core syntax
-- they are inserted here to handle #include
-- a Body is redefined to be a list of assignments or included content

statement :: ParserIO [Assignment]
statement = do { as <- assignment; return [as] }
	<|> do { enterInclude; as <- statements; leaveInclude; return as }

statements :: ParserIO [Assignment]
statements = do { ass <- statement `sepBy` m_whiteSpace; return (concat ass) }

-- B ::= [S]
body :: ParserIO Body
body = do { as <- statements ; return (Body as) }

-- P ::= R | { B }
prototype :: ParserIO Prototype
prototype = do { ref <- reference ; return (RefProto ref) }
	<|> do { b <- m_braces body ; return (BodyProto b) }

-- SF ::= B <eof>
specification :: ParserIO Body
specification = do { m_whiteSpace; b <- body ; eof; return b }

{------------------------------------------------------------------------------
    parser state
------------------------------------------------------------------------------}

-- the parser state is currently used only to maintain the stack of
-- open included files. but we may want to add other stuff later ...

data ParserState = ParserState
	{ includeStack :: [IncludeState]
	}

initialState = ParserState
	{ includeStack = []
	}

{------------------------------------------------------------------------------
    #include file handling (not part of core language)
------------------------------------------------------------------------------}

-- this extension to the core language implements #include
-- there is no attempt to maintain strict compatibility with the production SF compiler
-- (I'm not even sure of the exact syntax/semantics of #include in the production compiler)
-- in particular ...
-- (*) #include does not need to start in the first column
-- (*) it is integrated with the language syntax, so it is only valid where an "assignment"
--     would be valid. unlike cpp, this makes it illegal to have include files which do not
--     nest properly with the language blocks -- this is good (I think)
-- (*) relative pathnames are interpreted relative the directory of the including file
-- (*) It does not implement <filename> for locating files in a system search path

-- relative pathnames are interpreted relative to the including file
includePath :: String -> String -> String
includePath parentPath filePath = combine (takeDirectory parentPath) filePath

-- when we include a file, push:
-- the remaining source stream
-- and the including file position

type IncludeState = (String,SourcePos)

pushInclude :: ParserState -> IncludeState -> ParserState
pushInclude state i = state { includeStack = i:(includeStack state) }

popInclude :: ParserState -> (IncludeState,ParserState)
popInclude state = case (includeStack state) of
	[] -> error "attempt to pop empty #include stack: impossible!"
	(i:is) -> (i, state { includeStack = is } )

-- use this to check for recursive includes
alreadyIncluded :: ParserState -> String -> Bool
alreadyIncluded state path = path `elem` (map (sourceName . snd) (includeStack state))

enterInclude :: ParserIO ()
enterInclude = do
	m_symbol "#include"; path <- m_stringLiteral; m_semi
	currentPos <- getPosition
	let absolutePath = includePath (sourceName currentPos) path
	contentOrError <- liftIO (Control.Exception.try (readFile absolutePath))
	state <- getState
	case contentOrError of
		Left err -> fail ("can't open included file " ++ (show (err :: IOException)))
		Right content -> if (alreadyIncluded state absolutePath)
			then fail ("recursive #include: " ++ absolutePath)
			else switchInput content absolutePath

switchInput :: String -> String -> ParserIO ()
switchInput content path = do
	state <- getState
	input <- getInput
	pos <- getPosition
	setState (pushInclude state (input,pos))
	setPosition (newPos path 1 1)
	setInput content
	m_whiteSpace

leaveInclude :: ParserIO ()
leaveInclude = do
	eof
	state <- getState
	let ((input,pos),newState) = popInclude state
	setState newState
	setPosition pos
	setInput input

{------------------------------------------------------------------------------
    parse tree rendering (pretty printing)
------------------------------------------------------------------------------}

class ParseItem a where
	render :: a -> String
	
instance ParseItem Identifier where
	render (Identifier id) = id
instance ParseItem Reference where
	render (Reference ids) = (intercalate ":" (map render ids))
instance ParseItem Body where
	render (Body as) = intercalate "\n" (map render as)
instance ParseItem Assignment where
	render (Assignment ref val) = (render ref) ++ " " ++ (render val)
instance ParseItem Prototype where
	render (RefProto ref) = render ref
	render (BodyProto body) = "{" ++ bodyContents ++ "}"
		where bodyContents = (indentBlock (render body))
instance ParseItem Value where
	render (BasicValue bv) = (render bv) ++ ";"
	render (LinkValue ref) = (render ref) ++ ";"
	render (ProtoValue ps) = "extends " ++ (intercalate ", " (map render ps))
instance ParseItem BasicValue where
	render (BoolValue True) = "true"
	render (BoolValue False) = "false"
	render (NumValue n) = show n
	render (StringValue str) = show str
	render (NullValue) =  "NULL"
	render (DataRef ids) = "DATA " ++ (intercalate ":" (map render ids))
	render (Vector bvs) = "[" ++ (intercalate "," (map render bvs)) ++ "]"

{------------------------------------------------------------------------------
    parse SF specification
------------------------------------------------------------------------------}

parseSF :: String -> String -> IO (Either ParseError Body)
parseSF sourcePath source = runParserT specification initialState sourcePath source
