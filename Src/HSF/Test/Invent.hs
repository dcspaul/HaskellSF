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

noFail :: (a -> StoreOrError) -> (a -> Store)
noFail f = \x -> case (f x) of
		Left e -> error "impossible fail!"
		Right s -> s

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
	let s' = (noFail sfInherit)(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ([]) = \(ns,r,s) -> return ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> RandomResult Value

inventValue (BasicValue bv) = \(ns,r,s) -> do
	let s' = (noFail sfBind)(s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	v' <- inventLinkRef ns s
	inventValue v' $ (ns,r,s)

inventValue (LinkValue lr) = \(ns,r,s) -> do
	let (ns',v') = case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> (n,v)
	let s' = (noFail sfBind)(s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	let s' = (noFail sfBind)(s,r,SubStore (Store []))
	result <- inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

inventAssignment :: Assignment -> (NameSpace,Store) -> RandomResult Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = \(ns,s) -> do
	r <- inventLHSRef ns s
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

inventLHSId :: State StdGen Reference
-- invent a random ID
inventLHSId = do
	n <- randomV
	return (Reference [Identifier (randomId n)]) where

randomId :: Int -> String
randomId i = ids !! (i `mod` (length ids))
	where ids = map (:[]) ['a' .. 'z']

inventLHSRef :: NameSpace -> Store -> State StdGen Reference
inventLHSRef ns s = inventLHSId
-- inventLHSRef ns s = (Reference [Identifier "fooREF"])
-- look in the store for a random reference
-- if you can't find anything, use inventLHSId

inventProtoRef :: NameSpace -> Store -> State StdGen Prototype
inventProtoRef ns s = do
	return (BodyProto (Body []))
-- inventProtoRef ns s = (RefProto (Reference [Identifier "fooPROTO"]))
-- look in the store for a random reference to a prototype
-- if you can't find one, then generate an empty body

inventLinkRef :: NameSpace -> Store -> State StdGen Value
inventLinkRef ns s = do
	return (BasicValue (StringValue "frobble!"))
-- inventLinkRef ns s = (LinkValue (Reference [Identifier "fooLink"]))
-- look in the store for a random reference to a prototype or a value
-- if you can't find one, generate some arbitrary value
	





