{------------------------------------------------------------------------------
    SmartFrog evaluator
------------------------------------------------------------------------------}

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Cmd(rawSystem)
import System.FilePath.Posix
import GHC.IO.Exception

-- Parsec
import Text.Parsec (sepBy, sepBy1, (<|>), eof)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- cabal install MissingH
import Data.List.Utils (addToAL)
import Data.String.Utils (join,replace)

-- cabal install Safe
import Safe (initSafe)

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

{------------------------------------------------------------------------------
    lexer
------------------------------------------------------------------------------}

lexer = P.makeTokenParser emptyDef {
	P.reservedNames = ["true", "false","NULL","DATA","extends"],
	P.reservedOpNames = ["{","}","[","]"],
	P.commentStart = "/*",
	P.commentEnd = "*/",
	P.commentLine = "//"
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
    parse tree rendering
------------------------------------------------------------------------------}

tabString :: Int -> String
tabString n = foldl1 (++) (replicate n " ")

indentBlockBy :: String -> String -> String
indentBlockBy ts text
	| length text == 0		= text
	| otherwise				= "\n" ++ ts ++ (replace "\n" ("\n" ++ ts) text) ++ "\n"

indentBlock :: String -> String
indentBlock = indentBlockBy (tabString 2)

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
    store
------------------------------------------------------------------------------}

-- the store is implemented strictly as in the semantics from the paper 
-- ie. as hierarchical lists. this means that the result preserves the
-- ordering defined in the semantics, although I don't believe that this
-- is significant (or the same as the production compiler?)
-- The JSON standard also says (I think) that the order is records is not significant

data StoreValue = StoreValue BasicValue | SubStore Store deriving(Eq)
data Store = Store [(Identifier,StoreValue)] deriving(Eq)

{------------------------------------------------------------------------------
    semantic functions
------------------------------------------------------------------------------}

type NameSpace = Reference
type ErrorMessage = String
type Result = Either ErrorMessage Store

-- 6.7
-- this operator concatenates two references
-- the version in the paper is rather more complicated because it
-- accepts single identifiers as well, but I think this is only used in one case
-- so we do the conversion when it is called

(|+|) :: Reference -> Reference -> Reference
(|+|) (Reference r1) (Reference r2) = (Reference (r1 ++ r2))

-- 6.11
-- this function returns the longest strict prefix of the given reference

sfPrefix :: Reference -> Reference
sfPrefix (Reference r) = (Reference (initSafe r))

-- 6.12
-- this function updates the value of an identifier in a store,
-- or adds it if it does not already exist.
-- notice that this operates only on single identifiers --
-- the following function (bind) extends this to support hierarchical references

sfPut :: (Store,Identifier,StoreValue) -> Store
sfPut ( Store s, i, v ) = Store ( addToAL s i v )

-- 6.13
-- this function updates the value of a reference in a store
-- return an error if an attempt is made to update a reference whose parent does not exist,
-- or whose parent is not itself a store, or if we are attempting to replace the root store

sfBind :: (Store,Reference,StoreValue) -> Result
sfBind ( Store ivs, Reference is, v ) = sfBind' ivs is v where
	sfBind' _   []  _    = Left "[error 3] attempt to replace root store"
	sfBind' ivs [i] v    = Right (sfPut (Store ivs,i,v))
	sfBind' ivs (i:is) v =
		case (lookup i ivs) of
			Nothing -> Left ( "[error 2] reference has no parent: " ++ (render (Reference (i:is))))
			Just (StoreValue _) -> Left ( "[error 1] parent not a store: " ++ (render (Reference (i:is))))
			Just (SubStore (Store ivs')) -> do
				s' <- sfBind' ivs' is v
				return (Store (addToAL ivs i (SubStore s')))

-- 6.14
-- this function looks up the value of a reference in a store
-- return Nothing if the target is not found

sfFind :: (Store,Reference) -> Maybe StoreValue
sfFind ( Store ivs, Reference is ) = sfFind' ivs is where
	sfFind' ivs [] = Just (SubStore (Store ivs))
	sfFind' []  _  = Nothing
	sfFind' ivs (i:is) =
		case (lookup i ivs) of
			Nothing -> Nothing
			Just (StoreValue v) -> if null is then Just (StoreValue v) else Nothing
			Just (SubStore (Store ivs')) -> sfFind' ivs' is

-- 6.16
-- this function looks up a reference in a store, by starting with a given namespace
-- (reference of the sub-store) and searching up the hierarchy of parent stores until
-- a value is found (or not). It returns a tuple (ns,v) where ns is the namespace
-- in which the target element is found and v is the value.
-- return Nothing if the target is not found

sfResolv :: (Store,NameSpace,Reference) -> Maybe (NameSpace,StoreValue)
sfResolv (s, ns@(Reference is), r)
	| is == []			= maybePair (Reference [], sfFind(s,r))
	| v == Nothing 		= sfResolv (s, sfPrefix ns, r)
	| otherwise 		= maybePair (ns, v)
	where
		v = sfFind (s, ns |+| r)
		maybePair (a,Nothing) = Nothing
		maybePair (a,Just b) = Just (a,b)

-- 6.17
-- this function copies every attribute from the second store to the first store at
-- the given prefix. return an error if the underlying bind returns an error

sfCopy :: (Store,Store,Reference) -> Result
sfCopy ( s1, Store [], pfx ) = Right s1
sfCopy ( s1, Store ((i,v):s2), pfx ) = do
	s' <- sfBind( s1, pfx |+| (Reference [i]), v)
	sfCopy (s',Store s2,pfx)

-- 6.18
-- this function copies values from a given prototype to the target store
-- the prototype may be located in a higher-level namespace, hence the use of
-- resolve to locate the corresponding store

sfInherit :: (Store,NameSpace,Reference,Reference) -> Result
sfInherit (s, ns, p, r) =
	case (sfResolv(s,ns,p)) of
		Nothing -> Left ( "[error 4] can't resolve prototype: " ++ (render p) )
		Just (_, SubStore s') -> sfCopy(s,s',r)
		Just (_, StoreValue _) -> Left ( "[error 4] prototype is not a store: " ++ (render p) )

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

-- 6.23
-- A prototype is a sequence of bodies or references.
-- Bodies are evaluated directly, while references are first resolved
-- (in the current context) and then evaluated.
-- Composition proceeds right-to-left (since defined values override
-- any corresponding values in an extended prototype).

evalProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> Result

evalProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	fB <- evalBody bp $ (r,s)
	evalProtoList ps $ (ns, r, fB)

evalProtoList ((RefProto rp):ps) = \(ns,r,s) -> do
	s' <- sfInherit(s,ns,rp,r)
	evalProtoList ps $ (ns, r, s')

evalProtoList ([]) = \(ns,r,s) -> (Right s)

-- 6.24
-- A value is either a basic value, a prototype, or a link reference.
-- Basic values are entered directly in the store.
-- Prototypes are first evaluated, and link references are first resolved.

evalValue :: Value -> (NameSpace,Reference,Store) -> Result

evalValue (BasicValue bv) = \(ns,r,s) -> sfBind(s, r, StoreValue bv)

evalValue (LinkValue lr) = \(ns,r,s) -> do
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> Left ( "[error 5] can't resolve link value: " ++ (render lr) )
		Just (n,v) -> Right (n,v)
	sfBind(s, r, v')

evalValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	evalProtoList ps $ (ns,r,s')
	
-- 6.25
-- To assign a value to a reference, the store entry for the
-- reference is updated to contain the value.
-- Error 6 occurs if the prefix of the target reference is not an object.

evalAssignment :: Assignment -> (NameSpace,Store) -> Result

evalAssignment (Assignment r@(Reference [_]) v) = \(ns,s) -> do
	evalValue v $ (ns, (ns |+| r), s)

evalAssignment (Assignment r v) = \(ns,s) -> do
	case (sfResolv (s,ns,(sfPrefix r))) of
		Nothing -> Left ( "[error 6] can't resolve reference: " ++ (render r) )
		Just (_, StoreValue _) -> Left ( "[error 6] reference not an object: " ++ (render r) )
		Just (ns', _) -> evalValue v $ (ns, ns' |+| r, s)

-- 6.26
-- A body is a sequence of assignments.
-- These are recursively evaluated left-to-right with the store resulting
-- from one assignment being used as input to the next assignment.

evalBody :: Body -> (NameSpace,Store) -> Result

evalBody (Body (a:b)) = \(ns,s) -> do
	fA <- evalAssignment a $ (ns,s)
	evalBody (Body b) $ (ns,fA)

evalBody (Body []) = \(ns,s) -> (Right s)

-- 6.27
-- A complete SFSpecification is evaluated as a body, in the context of an empty store
-- and a reference to the root namespace.
-- The evaluation of the main sfConfig component is returned & other components are ignored.
-- It is an error if the main sfConfig element is not a store (eg., if it is a basic value).
	
evalSpecification :: Body -> Result

evalSpecification b = do
	fB <- evalBody b $ (Reference [], Store [])
	case (sfFind(fB,Reference [Identifier "sfConfig"])) of
		Nothing -> Left "no sfConfig at top level of specification"
		Just (StoreValue bv) -> Left ( "sfConfig cannot be a basic value: " ++ (render bv) )
		Just (SubStore s) -> return s

{------------------------------------------------------------------------------
    store rendering
------------------------------------------------------------------------------}

class StoreItem a where
	renderJSON :: a -> String 
	renderHerryJSON :: a -> String
	
instance StoreItem Identifier where
	renderJSON (Identifier i) = id i
	renderHerryJSON (Identifier i) = "\"" ++ (id i) ++ "\""
instance StoreItem Store where
	renderJSON (Store as) =
		(intercalate ",\n" (map renderJSONEntry as))
		where
			renderJSONEntry (i, StoreValue bv) = (renderJSON i) ++ ": " ++ (renderJSON bv)
			renderJSONEntry (i, SubStore s) = (renderJSON i) ++ ": {" ++ (indentBlock $ renderJSON s)  ++ "}"
	renderHerryJSON (Store as) =
		"{" ++ (intercalate "," (map renderHerryJSONEntry as)) ++ "}"
		where
			renderHerryJSONEntry (i, StoreValue bv) = (renderHerryJSON i) ++ ":" ++ (renderHerryJSON bv)
			renderHerryJSONEntry (i, SubStore s) = (renderHerryJSON i) ++ ":" ++ (renderHerryJSON s)
instance StoreItem BasicValue where
	renderJSON (BoolValue True) = "true"
	renderJSON (BoolValue False) = "false"
	renderJSON (NumValue n) = show n
	renderJSON (StringValue str) = show str
	renderJSON (NullValue) = "Null"
	renderJSON (DataRef ids) = intercalate ":" $ map renderJSON ids
	renderJSON (Vector bvs) = "[" ++ (intercalate ", " $ map renderJSON bvs) ++ "]"
	-- this version puts each element on a new line
	-- renderJSON (Vector bvs) = "[" ++ (indentBlock (intercalate ",\n" (map renderJSON bvs)))  ++ "]"
	renderHerryJSON (BoolValue True) = "true"
	renderHerryJSON (BoolValue False) = "false"
	renderHerryJSON (NumValue n) = show n
	renderHerryJSON (StringValue str) = show str
	renderHerryJSON (NullValue) = "Null"
	renderHerryJSON (DataRef ids) = intercalate ":" $ map renderJSON ids
	renderHerryJSON (Vector bvs) = "List(" ++ (intercalate ", " $ map renderJSON bvs) ++ ")"

{------------------------------------------------------------------------------
    compile things using paul's compiler or herry's compiler
------------------------------------------------------------------------------}

outputPath :: String -> String -> String
outputPath sourcePath who =
	(replaceFileName (takeDirectory sourcePath) "Tmp") </> who </>
		(replaceExtension (takeFileName sourcePath) ".json")

scriptPath :: String -> String
scriptPath sourceFile =
	(takeDirectory (takeDirectory sourceFile)) </> "herryparser.sh"	

herryCompile :: String -> IO ()
herryCompile sourcePath = do
 	exitCode <- rawSystem (scriptPath sourcePath) [ sourcePath, (outputPath sourcePath "Herry") ]
	return ()

paulCompile :: String -> IO ()
paulCompile sourcePath = do
	parseResult <- parseFromFile specification sourcePath
	writeFile (outputPath sourcePath "Paul") (output parseResult)
	where output parseResult = case (parseResult) of
		Left err  -> "** SF parser failed: " ++ sourcePath ++ "\n" ++ (show err)
		Right body -> case (evalSpecification body) of
			Left errorMessage -> "** SF evaluation failed: " ++ sourcePath ++ "\n" ++
										  errorMessage ++ "\n\n" ++ render body
			Right store -> renderHerryJSON store

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

compile :: String -> IO ()
compile sourcePath = do
	herryCompile sourcePath
	paulCompile sourcePath

main = getArgs >>= ( mapM compile )















