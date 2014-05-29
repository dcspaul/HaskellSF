{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Store management
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Store
	( Store(..), StoreValue(..), NameSpace, StoreOrError
	, (|+|), sfPrefix, sfPut, sfBind, sfFind, sfResolv, sfCopy, sfInherit
	, renderJSON, renderCompactJSON, renderHPSF
	) where

import HSF.Parser
import HSF.Errors
import HSF.Utils
import Data.List (intercalate)

-- cabal install Safe
import Safe (initSafe)

{------------------------------------------------------------------------------
    store
------------------------------------------------------------------------------}

-- the store is implemented strictly as in the semantics from the paper 
-- ie. as hierarchical lists. this means that the result preserves the
-- ordering defined in the semantics.
-- (note that the JSON standard says that the order of records is not significant)

data StoreValue = StoreValue BasicValue | SubStore Store deriving(Eq,Show)
data Store = Store [(Identifier,StoreValue)] deriving(Eq,Show)

{------------------------------------------------------------------------------
    semantic functions
------------------------------------------------------------------------------}

type NameSpace = Reference
type StoreOrError = Either Error Store

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
sfPut ( Store s, i, v ) = Store ( addToEndOfAL s i v )

-- 6.13
-- this function updates the value of a reference in a store
-- return an error if an attempt is made to update a reference whose parent does not exist,
-- or whose parent is not itself a store, or if we are attempting to replace the root store

sfBind :: (Store,Reference,StoreValue) -> StoreOrError
sfBind ( Store ivs, Reference is, v ) = sfBind' ivs is v where
	sfBind' _   []  _    = Left EREPLACEROOTSTORE
	sfBind' ivs [i] v    = Right (sfPut (Store ivs,i,v))
	sfBind' ivs (i:is) v =
		case (lookup i ivs) of
			Nothing -> Left ( ENOPARENT (render (Reference (i:is))) )
			Just (StoreValue _) -> Left ( EPARENTNOTSTORE (render (Reference (i:is))) )
			Just (SubStore (Store ivs')) -> do
				s' <- sfBind' ivs' is v
				return (Store (addToEndOfAL ivs i (SubStore s'))) where

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
		Nothing -> Left ( ENOPROTO (render p) )
		Just (_, SubStore s') -> sfCopy(s,s',r)
		Just (_, StoreValue _) -> Left ( EPROTONOTSTORE (render p) )

{------------------------------------------------------------------------------
    store rendering
------------------------------------------------------------------------------}

-- two versions of the JSON store rendering ...
-- the compact one is compatible with the scala compiler (so we can compare them)
-- the other one is more suitable for human consumption

class StoreItem a where
	renderJSON :: a -> String 
	renderCompactJSON :: a -> String
	renderHPSF :: a -> String
	
instance StoreItem Identifier where

	renderHPSF (Identifier i) = id i
	renderJSON (Identifier i) = id i
	renderCompactJSON (Identifier i) = "\"" ++ (id i) ++ "\""

instance StoreItem Store where
	
	renderHPSF (Store as) = (intercalate "\n" (map renderHPEntry as)) where
		renderHPEntry (i, StoreValue bv) = (renderHPSF i) ++ " " ++ (renderHPSF bv) ++ ";"
		renderHPEntry (i, SubStore s) = (renderHPSF i) ++ " extends  {" ++ (indentBlock $ renderHPSF s)  ++ "}"
		
	renderJSON (Store as) = (intercalate ",\n" (map renderJSONEntry as)) where
		renderJSONEntry (i, StoreValue bv) = (renderJSON i) ++ ": " ++ (renderJSON bv)
		renderJSONEntry (i, SubStore s) = (renderJSON i) ++ ": {" ++ (indentBlock $ renderJSON s)  ++ "}"
		
	renderCompactJSON (Store as) = "{" ++ (intercalate "," (map renderCompactJSONEntry as)) ++ "}" where
		renderCompactJSONEntry (i, StoreValue bv) = (renderCompactJSON i) ++ ":" ++ (renderCompactJSON bv)
		renderCompactJSONEntry (i, SubStore s) = (renderCompactJSON i) ++ ":" ++ (renderCompactJSON s)

instance StoreItem BasicValue where
	
	renderHPSF (BoolValue True) = "true"
	renderHPSF (BoolValue False) = "false"
	renderHPSF (NumValue n) = show n
	renderHPSF (StringValue str) = show str
	renderHPSF (NullValue) = "Null"
	renderHPSF (DataRef ids) = "DATA " ++ (intercalate ":" $ map renderHPSF ids)
	renderHPSF (Vector bvs) = "[|" ++ (intercalate ", " $ map renderHPSF bvs) ++ "|]"
		
	renderJSON (BoolValue True) = "true"
	renderJSON (BoolValue False) = "false"
	renderJSON (NumValue n) = show n
	renderJSON (StringValue str) = show str
	renderJSON (NullValue) = "Null"
	renderJSON (DataRef ids) = intercalate ":" $ map renderHPSF ids
	renderJSON (Vector bvs) = "[" ++ (intercalate ", " $ map renderHPSF bvs) ++ "]"
	
	-- this version puts each element on a new line
	-- renderJSON (Vector bvs) = "[" ++ (indentBlock (intercalate ",\n" (map renderJSON bvs)))  ++ "]"
	renderCompactJSON (BoolValue True) = "true"
	renderCompactJSON (BoolValue False) = "false"
	renderCompactJSON (NumValue n) = show n
	renderCompactJSON (StringValue str) = show str
	renderCompactJSON (NullValue) = "Null"
	renderCompactJSON (DataRef ids) = "\"$." ++ ( intercalate ":" $ map renderJSON ids ) ++ "\""
	renderCompactJSON (Vector bvs) = "[" ++ (intercalate "," $ map renderCompactJSON bvs) ++ "]"
	
instance StoreItem StoreValue where
	
	renderHPSF (StoreValue bv) = (renderHPSF bv)
	renderHPSF (SubStore s) = "{" ++ (indentBlock $ renderHPSF s)  ++ "}"

	renderJSON (StoreValue bv) = (renderJSON bv)
	renderJSON (SubStore s) = "{" ++ (indentBlock $ renderJSON s)  ++ "}"

	renderCompactJSON (StoreValue bv) = (renderCompactJSON bv)
	renderCompactJSON (SubStore s) = "{" ++ (renderCompactJSON s)  ++ "}"
