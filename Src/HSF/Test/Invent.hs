{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Invent plausible random references
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.Invent
	( inventSF
	) where

import HSF.Store
import HSF.Parser
import HSF.Errors
import HSF.Utils
import HSF.Test.Frequencies

import System.Random
import Control.Monad.State

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

-- these functions mirror the semantic evaluation functions
-- in this case, as well are populating the store,
-- we also create a copy of the parse tree in which the
-- placeholders have been replaced with arbitrary values
-- the store is then used to select arbitrary values which are legal

data Result a = Result
	{ store :: Store
	, node :: a
	}

-- the state monad propagates the random number generator

randomV :: (RandomGen g, Random a) => State g a
randomV = state random

type RandomResult a = State StdGen (Result a)	

inventProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> RandomResult [Prototype]

-- invent a reference to a random prototype 
inventProtoList ((RefProto (Reference [Identifier "?ref"])):ps) = \(ns,r,s) -> do
	rp <- inventProtoRef ns s
	inventProtoList (rp:ps) $ (ns,r,s)

-- a body prototype
inventProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	fB <- inventBody bp $ (r,s)
	result <- inventProtoList ps $ (ns, r, (store fB))
	return ( result { node =  (BodyProto (node fB)):(node result) } )

-- a reference to a prototype
inventProtoList ((RefProto rp):ps) = \(ns,r,s) -> do
	let s' = iInherit(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ([]) = \(ns,r,s) -> return ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> RandomResult Value

inventValue (BasicValue bv) = \(ns,r,s) -> do
	let s' = (iBind "bv")(s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	v' <- inventLinkRef ns s
	inventValue v' $ (ns,r,s)

inventValue (LinkValue lr) = \(ns,r,s) -> do
	let (ns',v') = case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> (n,v)
	let s' = (iBind "lv")(s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	let s' = (iBind "pv")(s,r,SubStore (Store []))
	result <- trimProtoList $ inventProtoList ps $ (ns,r,s')
	-- result <- inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

-- remove excess empty blocks from the prototype list
trimProtoList :: RandomResult [Prototype] -> RandomResult [Prototype]
trimProtoList ps = do
	result <- ps
	return result { node = trimProtoList' (node result) }
	where
		trimProtoList' ps = if (null trimmed) then [BodyProto (Body [])] else trimmed
			where trimmed = trimProtoList'' ps
		trimProtoList'' [] = []
		trimProtoList'' ((BodyProto (Body [])):ps) = (trimProtoList'' ps)
		-- trimProtoList' ((BodyProto (Body [])):ps) = (BodyProto (Body [Assignment (Reference [Identifier "FOO"])  (BasicValue (NumValue 99)) ])):(trimProtoList' ps)
		trimProtoList'' (p:ps) = p:(trimProtoList'' ps)

inventAssignment :: Assignment -> (NameSpace,Store) -> RandomResult Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = \(ns,s) -> do
	r <- inventLHSRef ns s
	-- "r" instead of "ns |+| r" here, because inventLHSRef returns a full pathname
	result <- inventValue v $ (ns, r, s)
	return ( result { node = Assignment r (node result) } )
	
inventAssignment (Assignment r v) = \(ns,s) -> do
	result <- inventValue v $ (ns, (ns |+| r), s)
	return ( result { node = Assignment r (node result) } )

inventBody :: Body -> (NameSpace,Store) -> RandomResult Body

inventBody (Body (a:b)) = \(ns,s) -> do
	fA <- inventAssignment a $ (ns,s)
	result <- inventBody (Body b) $ (ns,(store fA))
	let (Body as) = node result
	return ( result { node = Body ((node fA):as) } )

inventBody (Body []) = \(ns,s) -> return ( Result { store = s, node = (Body []) } )

inventSF :: SFConfig -> SFConfig
 
inventSF (SFConfig as) = (SFConfig as') where
	(x,y) = runState (inventBody (Body as) $ (Reference [], Store [])) (mkStdGen 33)
	(Body as') = node x

{------------------------------------------------------------------------------
    invent values for placeholders
------------------------------------------------------------------------------}

-- these routines use the store to invent plausible values for placeholders

-- TODO: at some point, we should (partially, randomly) strip the namespace
-- from the full paths that we return (on both the lhs & rhs)

-- TODO: sometimes we should generate illegal things

-- invent a reference to a random variable
inventLHSId :: State StdGen Reference
inventLHSId = do
	r <- randomV
	let candidates = map (:[]) ['a' .. 'z']
	let n = length candidates
	return (Reference [Identifier (candidates !! (r `mod` n))]) where

-- invent a reference for the LHS (placement?)
-- eg: REF "stuff"
-- if there is none, return an arbitrary variable
inventLHSRef :: NameSpace -> Store -> State StdGen Reference
inventLHSRef ns s = do
	r <- randomV
	let candidates = (blockRefs s) ++ (valueRefs s)
	let n = length candidates
	if (n > 0)
		then return (candidates !! (r `mod` n))
		else inventLHSId

-- invent a reference to a prototype
-- eg: foo extends { .. }, REF, { ..} ...
-- if there is none, return an empty block
-- (most of these empty blocks get filtered out later)
inventProtoRef :: NameSpace -> Store -> State StdGen Prototype
inventProtoRef ns s = do
	r <- randomV
	let candidates = blockRefs s
	let n = length candidates
	if (n > 0)
		then return (RefProto (candidates !! (r `mod` n)))
		else return (BodyProto (Body []))

-- invent a link reference
-- eg: foo REF
-- if there is none, return a basic value
inventLinkRef :: NameSpace -> Store -> State StdGen Value
inventLinkRef ns s = do
	r <- randomV
	let candidates = (blockRefs s) ++ (valueRefs s)
	let n = length candidates
	if (n > 0)
		then return (LinkValue (candidates !! (r `mod` n)))
		else return (BasicValue (StringValue "noref"))

{------------------------------------------------------------------------------
    return list of candidate references from the store
------------------------------------------------------------------------------}

-- return a list of all valid value references (not blocks)
valueRefs :: Store -> [Reference]
valueRefs s = map Reference $ filter (\r -> (length r)>1) (valueRefs' [] s)
valueRefs' path (Store []) = []
valueRefs' path (Store ((Identifier i, StoreValue _):s')) =
	(path++[Identifier i]):(valueRefs' path (Store s'))
valueRefs' path (Store ((Identifier i, SubStore s):s')) =
	(valueRefs' (path++[Identifier i]) s) ++ (valueRefs' path (Store s'))

-- return a list of all valid block references (not basic values)
blockRefs :: Store -> [Reference]
blockRefs s = map Reference $ filter (\r -> (length r)>1) (blockRefs' [] s)
blockRefs' path (Store []) = []
blockRefs' path (Store ((Identifier i, StoreValue _):s')) = blockRefs' path (Store s')
blockRefs' path (Store ((Identifier i, SubStore s):s')) =
	(path++[Identifier i]) : 
	( (blockRefs' (path++[Identifier i]) s) ++ (blockRefs' path (Store s')) )

{------------------------------------------------------------------------------
    store functions
------------------------------------------------------------------------------}

-- these routines ignore errors when attempting to put values into the store
-- this can happen if (for example) a reference on the LHS is used to
-- overwrite the current block (or a parent) - see test t1-error1, for example
-- in this case, we simply ignore the item that we are trying to put in the store
-- it will never be accessed, and the resulting code will fail to compile anyway
-- TODO: I guess we should try to avoid this (or at least control the frequency
-- with which it happens) by not generating LHS references which are in the current
-- path .... 

iInherit :: (Store,NameSpace,Reference,Reference) -> Store
iInherit (s,n,r,r') = case (sfInherit (s,n,r,r')) of
	Left e -> s
	Right s' -> s'

iBind :: String -> (Store,Reference,StoreValue) -> Store
iBind i (s,r,v) = case (sfBind (s,r,v)) of
	Left e -> s
	Right s' -> s'
