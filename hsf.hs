{------------------------------------------------------------------------------
    SmartFrog evaluator
------------------------------------------------------------------------------}

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Cmd(rawSystem)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith,ExitCode(..))
import System.Environment (getArgs,getExecutablePath,lookupEnv)
import System.FilePath.Posix
import GHC.IO.Exception
import System.Console.GetOpt

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
    parse tree rendering (pretty printing)
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
type ErrorFn = Bool -> String
type StoreOrError = Either ErrorFn Store

-- the Data.List.Utils version of addToAL adds new items at the start of the alist
-- this version follows the strict semantics by adding them at the end

addToAL' [] i v = [(i,v)]
addToAL' ((i',v'):s') i v
	| (i'==i)	= (i,v):s'
	| otherwise = (i',v'):(addToAL' s' i v)

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
sfPut ( Store s, i, v ) = Store ( addToAL' s i v )

-- 6.13
-- this function updates the value of a reference in a store
-- return an error if an attempt is made to update a reference whose parent does not exist,
-- or whose parent is not itself a store, or if we are attempting to replace the root store

sfBind :: (Store,Reference,StoreValue) -> StoreOrError
sfBind ( Store ivs, Reference is, v ) = sfBind' ivs is v where
	sfBind' _   []  _    = Left ( err EREPLACEROOTSTORE [] )
	sfBind' ivs [i] v    = Right (sfPut (Store ivs,i,v))
	sfBind' ivs (i:is) v =
		case (lookup i ivs) of
			Nothing -> Left ( err ENOPARENT [ render (Reference (i:is)) ] )
			Just (StoreValue _) -> Left ( err EPARENTNOTSTORE [ render (Reference (i:is)) ] )
			Just (SubStore (Store ivs')) -> do
				s' <- sfBind' ivs' is v
				return (Store (addToAL' ivs i (SubStore s'))) where

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

sfCopy :: (Store,Store,Reference) -> StoreOrError
sfCopy ( s1, Store [], pfx ) = Right s1
sfCopy ( s1, Store ((i,v):s2), pfx ) = do
	s' <- sfBind( s1, pfx |+| (Reference [i]), v)
	sfCopy (s',Store s2,pfx)

-- 6.18
-- this function copies values from a given prototype to the target store
-- the prototype may be located in a higher-level namespace, hence the use of
-- resolve to locate the corresponding store

sfInherit :: (Store,NameSpace,Reference,Reference) -> StoreOrError
sfInherit (s, ns, p, r) =
	case (sfResolv(s,ns,p)) of
		Nothing -> Left ( err ENOPROTO [render p] )
		Just (_, SubStore s') -> sfCopy(s,s',r)
		Just (_, StoreValue _) -> Left ( err EPROTONOTSTORE [render p] )

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

-- 6.23
-- A prototype is a sequence of bodies or references.
-- Bodies are evaluated directly, while references are first resolved
-- (in the current context) and then evaluated.
-- Composition proceeds right-to-left (since defined values override
-- any corresponding values in an extended prototype).

evalProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> StoreOrError

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

evalValue :: Value -> (NameSpace,Reference,Store) -> StoreOrError

evalValue (BasicValue bv) = \(ns,r,s) -> sfBind(s, r, StoreValue bv)

evalValue (LinkValue lr) = \(ns,r,s) -> do
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> Left ( err ENOLR [render lr] )
		Just (n,v) -> Right (n,v)
	sfBind(s, r, v')

evalValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	evalProtoList ps $ (ns,r,s')
	
-- 6.25
-- To assign a value to a reference, the store entry for the
-- reference is updated to contain the value.
-- Error 6 occurs if the prefix of the target reference is not an object.

evalAssignment :: Assignment -> (NameSpace,Store) -> StoreOrError

evalAssignment (Assignment r@(Reference [_]) v) = \(ns,s) -> do
	evalValue v $ (ns, (ns |+| r), s)

evalAssignment (Assignment r v) = \(ns,s) -> do
	case (sfResolv (s,ns,(sfPrefix r))) of
		Nothing -> Left ( err EASSIGN [render r] )
		Just (_, StoreValue _) -> Left ( err EREFNOTOBJ [render r] )
		Just (ns', _) -> evalValue v $ (ns, ns' |+| r, s)

-- 6.26
-- A body is a sequence of assignments.
-- These are recursively evaluated left-to-right with the store resulting
-- from one assignment being used as input to the next assignment.

evalBody :: Body -> (NameSpace,Store) -> StoreOrError

evalBody (Body (a:b)) = \(ns,s) -> do
	fA <- evalAssignment a $ (ns,s)
	evalBody (Body b) $ (ns,fA)

evalBody (Body []) = \(ns,s) -> (Right s)

-- 6.27
-- A complete SFSpecification is evaluated as a body, in the context of an empty store
-- and a reference to the root namespace.
-- The evaluation of the main sfConfig component is returned & other components are ignored.
-- It is an error if the main sfConfig element is not a store (eg., if it is a basic value).
	
evalSpecification :: Body -> StoreOrError

evalSpecification b = do
	fB <- evalBody b $ (Reference [], Store [])
	case (sfFind(fB,Reference [Identifier "sfConfig"])) of
		Nothing -> Left ( err ENOSPEC [] )
		Just (StoreValue bv) -> Left ( err ESPEC [render bv] )
		Just (SubStore s) -> return s

{------------------------------------------------------------------------------
    store rendering
------------------------------------------------------------------------------}

-- two versions of the store rendering ...
-- the compact one is compatible with the scala compiler (so we can compare them)
-- the other one is more suitable for human consumption

class StoreItem a where
	renderJSON :: a -> String 
	renderCompactJSON :: a -> String
	
instance StoreItem Identifier where
	renderJSON (Identifier i) = id i
	renderCompactJSON (Identifier i) = "\"" ++ (id i) ++ "\""
instance StoreItem Store where
	renderJSON (Store as) = (intercalate ",\n" (map renderJSONEntry as)) where
		renderJSONEntry (i, StoreValue bv) = (renderJSON i) ++ ": " ++ (renderJSON bv)
		renderJSONEntry (i, SubStore s) = (renderJSON i) ++ ": {" ++ (indentBlock $ renderJSON s)  ++ "}"
	renderCompactJSON (Store as) = "{" ++ (intercalate "," (map renderCompactJSONEntry as)) ++ "}" where
		renderCompactJSONEntry (i, StoreValue bv) = (renderCompactJSON i) ++ ":" ++ (renderCompactJSON bv)
		renderCompactJSONEntry (i, SubStore s) = (renderCompactJSON i) ++ ":" ++ (renderCompactJSON s)
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
	renderCompactJSON (BoolValue True) = "true"
	renderCompactJSON (BoolValue False) = "false"
	renderCompactJSON (NumValue n) = show n
	renderCompactJSON (StringValue str) = show str
	renderCompactJSON (NullValue) = "Null"
	renderCompactJSON (DataRef ids) = "\"$." ++ ( intercalate ":" $ map renderJSON ids ) ++ "\""
	renderCompactJSON (Vector bvs) = "[" ++ (intercalate "," $ map renderCompactJSON bvs) ++ "]"

{------------------------------------------------------------------------------
    compile using the scala or haskell compiler
------------------------------------------------------------------------------}

sfParser :: String -> String -> String -> IO (String)
sfParser sourcePath destPath sfParserPath = do
	execPath <- getExecutablePath
	let scriptPath = (takeDirectory execPath) </> "runSfParser.sh"
 	exitCode <- rawSystem scriptPath [ sourcePath, destPath, sfParserPath  ]
	case (exitCode) of
		ExitSuccess -> readFile destPath
		ExitFailure code -> fail ("script failed: " ++ scriptPath ++
			 " " ++ sourcePath ++ " " ++ destPath ++ " " ++ sfParserPath )

compile :: Bool -> String -> String -> IO (String)
compile isComparing sourcePath destPath = do
	-- parse it & evaluate if the parse succeeds
	storeOrError <- parseFromFile specification sourcePath
	let result = case (storeOrError) of
		Left e -> Left $ err EPARSEFAIL [ (show e) ] $ isComparing
		Right body -> case (evalSpecification body) of
			Left error -> Left $ ( error $ isComparing ) ++ "\n"
			Right store -> Right $ ( renderStore store ) ++ "\n" where
				renderStore = if (isComparing) then renderCompactJSON else renderJSON
	-- of we are comparing outputs, put the error message in the file
	-- otherwise, print it to the stderr
	case (result) of
		Left e -> if (isComparing)
			then writeFile destPath e
			else hPutStrLn stderr ( "** " ++ sourcePath ++ "\n" ++ e )
		Right json -> if (destPath == "-")
			then putStr json
			else writeFile destPath json
	-- return the results or the error message
	case (result) of
		Left e -> return e
		Right json -> return json

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

-- err returns a function which gives the appropriate version of the message
-- depending on whether we are doing a comparison with sfparser or not

data ErrorCode = EPARSEFAIL | EPARENTNOTSTORE | ENOPARENT | EREPLACEROOTSTORE |
	ENOPROTO | EPROTONOTSTORE | ENOLR | EASSIGN | EREFNOTOBJ | ENOSPEC | ESPEC

err :: ErrorCode -> [String] -> Bool -> String
err code args = \isComparing -> case (code) of
	EPARSEFAIL	-> if (isComparing)
		then (id a) ++ "\n"
		else "parse failed: " ++ (id a) ++ "\n"
	EPARENTNOTSTORE -> "parent not a store [error 1]: " ++ a
	ENOPARENT -> "reference has no parent [error 2]: " ++ a
	EREPLACEROOTSTORE -> "attempt to replace root store [error 3]"
	ENOPROTO -> "can't resolve prototype [error 4]: " ++ a
	EPROTONOTSTORE -> "prototype is not a store [error 4]: " ++ a
	ENOLR -> if (isComparing)
		then "[err5] cannot find link reference " ++ a
		else "can't resolve link value [error 5]: " ++ a
	EASSIGN -> if (isComparing)
		then "[err6] prefix of " ++ a ++ " is not a component"
		else "can't resolve reference [error 6]: " ++ a
	EREFNOTOBJ -> "reference not an object [error 6]: " ++ a
	ENOSPEC -> if (isComparing)
		then "[err7] sfConfig is not exist or a component"
		else "no sfConfig at top level of specification [error 7]"
	ESPEC -> "sfConfig cannot be a basic value [error 7]: " ++ a
	where a = args!!0

{------------------------------------------------------------------------------
    option handling
------------------------------------------------------------------------------}

-- output directory arg:
-- with no slash, it is treated as a subdirectory of the source directory
-- you can use ".." in this to refer to directories above the source
-- with a slash it is treated as an absolute pathname
-- the default is "" which is the same directory as the source
-- you can use "-" for stdout

data Flag = Output String | Compare | SfParser String deriving(Show,Eq)

options :: [OptDescr Flag]
options =
	[ Option ['o'] ["output"]	(ReqArg Output "DIR")		"directory for json output"
	, Option ['c'] ["compare"]	(NoArg Compare)				"compare with output of Scala compiler"
	, Option ['s'] ["sfparser"]	(ReqArg SfParser "FILE")	"location of sfparser"
	]
 
parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(args,fs,[]) -> do
		return (args,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

outputDir :: [Flag] -> String
outputDir [] = ""
outputDir ((Output d):_) = d
outputDir (_:rest) = outputDir rest

findSfParserPath :: [Flag] -> IO (String)
findSfParserPath fs = do
	let arg = sfParserArg fs
	sfParserEnv <- lookupEnv "SFPARSER"
	case (arg) of
		Just f -> return f
		Nothing -> case (sfParserEnv) of
			Just s -> return s
			Nothing -> return "sfparser"

sfParserArg :: [Flag] -> Maybe String
sfParserArg [] = Nothing
sfParserArg ((SfParser f):_) = Just f
sfParserArg (_:rest) = sfParserArg rest

jsonPath :: String -> String -> String -> String 
jsonPath srcPath relativeDir ext =
	((takeDirectory srcPath) </> relativeDir </>
		(addExtension ((dropExtension (takeFileName srcPath)) ++ ext) ".json"))

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do
    (args, files) <- getArgs >>= parseOptions
    mapM_ (process args) files

process :: [Flag] -> String -> IO ()
process opts srcPath = do
	let dstPath = if ((outputDir opts) == "-")
		then (++) "-"
		else jsonPath srcPath (outputDir opts)
	if (Compare `elem` opts) then do
		sfParserPath <- (findSfParserPath opts)
		haskellResult <- compile True srcPath (dstPath "-1")
		scalaResult <- sfParser srcPath (dstPath "-2") sfParserPath
		if (haskellResult == scalaResult)
			then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			else putStr ( "** match failed: " ++ (takeBaseName srcPath) ++ "\n"
				++ "Haskell: " ++ haskellResult ++ "Scala:   " ++ scalaResult )
	else do
		compile False srcPath (dstPath "")
		return ()
	return ()
