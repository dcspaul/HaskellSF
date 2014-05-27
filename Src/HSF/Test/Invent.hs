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

import System.Random
import Control.Monad.State

{------------------------------------------------------------------------------
    new stuff
------------------------------------------------------------------------------}

-- type RandomResult a = Either Error (Result a)

randomV :: (RandomGen g, Random a) => State g a
randomV = state random

type RandomResult a = State StdGen (Result a)

noFail :: (Show a) => (a -> StoreOrError) -> String -> Store -> (a -> Store)
noFail f name s = \x -> case (f x) of
		Left e -> error (name ++ " fail!\nx: " ++ (show x) ++ "\ns: " ++ (show s))
		Right s' -> s'

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

data Result a = Result
	{ store :: Store
	, node :: a
	}

inventProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> RandomResult [Prototype]

-- invent a reference to a random prototype 
-- if we can't find one, generate an empty body prototype
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
	let s' = (noFail sfInherit "sfInherit" s)(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ([]) = \(ns,r,s) -> return ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> RandomResult Value

inventValue (BasicValue bv) = \(ns,r,s) -> do
	let s' = (noFail sfBind "sfBind" s)(s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	v' <- inventLinkRef ns s
	inventValue v' $ (ns,r,s)

inventValue (LinkValue lr) = \(ns,r,s) -> do
	let (ns',v') = case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> (n,v)
	let s' = (noFail sfBind "sfBind" s)(s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	let s' = (noFail sfBind "sfBind" s)(s,r,SubStore (Store []))
	result <- inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

inventAssignment :: Assignment -> (NameSpace,Store) -> RandomResult Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = \(ns,s) -> do
	r <- inventLHSRef ns s
	
	XXXXXXX
	-- TODO: this just wants to be r - not ns |+| r
	-- but only if we find a real reference
	-- if we give up and create a simple symbol, then we need to add the namespace ....
	
	result <- inventValue v $ (ns, (ns |+| r), s)
	return ( result { node = Assignment r (node result) } )
	
inventAssignment (Assignment (Reference [Identifier "?id"]) v) = \(ns,s) -> do
	r <- inventLHSId
	result <- inventValue v $ (ns, (ns |+| r), s)
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

----------------

-- TODO: at some point, we need to (partially, randomly) strip the absolute reference paths

inventLHSId :: State StdGen Reference
-- invent a random ID
inventLHSId = do
	r <- randomV
	let candidates = map (:[]) ['a' .. 'z']
	let n = length candidates
	return (Reference [Identifier (candidates !! (r `mod` n))]) where

inventLHSRef :: NameSpace -> Store -> State StdGen Reference
inventLHSRef ns s = do
	inventLHSId
{--
	r <- randomV
	let candidates = (blockRefs s) ++ (valueRefs s)
	let n = length candidates
	if (n > 0)
		then return (candidates !! (r `mod` n))
		else inventLHSId
--}

inventProtoRef :: NameSpace -> Store -> State StdGen Prototype
inventProtoRef ns s = do
	r <- randomV
	let candidates = blockRefs s
	let n = length candidates
	if (n > 0)
		then return (RefProto (candidates !! (r `mod` n)))
		else return (BodyProto (Body []))

inventLinkRef :: NameSpace -> Store -> State StdGen Value
inventLinkRef ns s = do
	r <- randomV
	let candidates = (blockRefs s) ++ (valueRefs s)
	let n = length candidates
	if (n > 0)
		then return (LinkValue (candidates !! (r `mod` n)))
		else return (BasicValue (StringValue "noref"))

-- inventLinkRef ns s = (LinkValue (Reference [Identifier "fooLink"]))
-- look in the store for a random reference to a prototype or a value
-- if you can't find one, generate some arbitrary value
	

-- data StoreValue = StoreValue BasicValue | SubStore Store deriving(Eq)
-- data Store = Store [(Identifier,StoreValue)] deriving(Eq)

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


