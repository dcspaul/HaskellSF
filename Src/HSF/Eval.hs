{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Evaluator
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Eval
	( evalSF
	, renderJSON
	, renderCompactJSON
	) where

import HSF.Parser
import HSF.Utils

import Data.List (intercalate)

-- cabal install Safe
import Safe (initSafe)

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
type ErrorFn = ErrorFormat -> String
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
	sfBind' _   []  _    = Left (err EREPLACEROOTSTORE)
	sfBind' ivs [i] v    = Right (sfPut (Store ivs,i,v))
	sfBind' ivs (i:is) v =
		case (lookup i ivs) of
			Nothing -> Left ( err (ENOPARENT (render (Reference (i:is)))) )
			Just (StoreValue _) -> Left ( err (EPARENTNOTSTORE (render (Reference (i:is)))) )
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
		Nothing -> Left ( err (ENOPROTO (render p)) )
		Just (_, SubStore s') -> sfCopy(s,s',r)
		Just (_, StoreValue _) -> Left ( err (EPROTONOTSTORE (render p)) )

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
		Nothing -> Left ( err (ENOLR (render lr)) )
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
		Nothing -> Left ( err (EASSIGN (render r)) )
		Just (_, StoreValue _) -> Left ( err (EREFNOTOBJ (render r)) )
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
	
evalSF :: Body -> StoreOrError

evalSF b = do
	fB <- evalBody b $ (Reference [], Store [])
	case (sfFind(fB,Reference [Identifier "sfConfig"])) of
		Nothing -> Left (err ENOSPEC)
		Just (StoreValue bv) -> Left ( err (ESPEC (render bv)) )
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

