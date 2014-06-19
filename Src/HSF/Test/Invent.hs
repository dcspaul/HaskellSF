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
import Data.Maybe

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

-- these functions mirror the semantic evaluation functions
-- in this case, as well are populating the store,
-- we also create a copy of the parse tree in which the
-- placeholders are replaced with random (valid) values from the store

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
	let s' = either (\_ -> s) id $ sfInherit(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ([]) = \(ns,r,s) -> return ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> RandomResult Value

-- notice that we ignore failures of sfbind (ie. they don't affect the store)
-- this shouldn't occur in a valid spec, but we may want
-- to autogenerate invalid specs for testing, in which case we might see this

inventValue (BasicValue bv) = \(ns,r,s) -> do
	let s' = either (\_ -> s) id $ sfBind (s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	v' <- inventLinkRef ns s
	inventValue v' $ (ns,r,s)

inventValue (LinkValue lr) = \(ns,r,s) -> do
	let (ns',v') = case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> (n,v)
	let s' = either (\_ -> s) id $ sfBind (s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	let s' = either (\_ -> s) id $ sfBind (s,r,SubStore (Store []))
	result <- trimProtoList $ inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

-- remove excess empty blocks from the prototype list
-- these tend to get generated if the quickcheck output contains placeholders
-- for block references which we can't instantiate because there are no suitable
-- values in the store

trimProtoList :: RandomResult [Prototype] -> RandomResult [Prototype]
trimProtoList ps = do
	result <- ps
	return result { node = trimProtoList' (node result) }
	where
		trimProtoList' ps = if (null trimmed) then [BodyProto (Body [])] else trimmed
			where trimmed = trimProtoList'' ps
		trimProtoList'' [] = []
		trimProtoList'' ((BodyProto (Body [])):ps) = (trimProtoList'' ps)
		trimProtoList'' (p:ps) = p:(trimProtoList'' ps)

inventAssignment :: Assignment -> (NameSpace,Store) -> RandomResult Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = \(ns,s) -> do
	r <- inventLHSRef ns s
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

{------------------------------------------------------------------------------
    invent values for placeholders
------------------------------------------------------------------------------}

-- these routines use the store to invent plausible values for placeholders

-- TODO: sometimes we should generate illegal things

-- invent a reference for the LHS of an assignment ("placement")
-- any reference in the store is suitable (whether it is a block or a value)
-- if there is none, invent an arbitrary id
inventLHSRef :: NameSpace -> Store -> State StdGen Reference
inventLHSRef ns s = fromMaybe inventLHSId (randomRefFromStore ns s) 
	where inventLHSId = do
		r <- randomV
		let candidates = map (:[]) ['a' .. 'z']
		let n = length candidates
		return (Reference [Identifier (candidates !! (r `mod` n))]) where

-- invent a reference to a prototype in an extension
-- eg: foo extends { .. }, REF, { ..} ...
-- if there is none, return an empty block (most of these get filtered out later)
inventProtoRef :: NameSpace -> Store -> State StdGen Prototype
inventProtoRef ns s = maybe emptyBody (liftM RefProto) (randomBlockRefFromStore ns s)
	where emptyBody = return (BodyProto (Body []))

-- invent a link reference for the RHS of an assignment
-- any reference in the store is suitable (whether it is a block or a value)
-- if there is none, return a basic value
inventLinkRef :: NameSpace -> Store -> State StdGen Value
inventLinkRef ns s = maybe noRef (liftM LinkValue) (randomRefFromStore ns s)
	where noRef = return (BasicValue (StringValue "noref"))

{------------------------------------------------------------------------------
    pick random references from the store
------------------------------------------------------------------------------}

-- return random block reference from store
randomBlockRefFromStore :: NameSpace -> Store -> Maybe (State StdGen Reference)
randomBlockRefFromStore ns s = randomRef $ filter isBlock $ filter isRef $ flattenStore $ subStoreAtNameSpace ns s

-- return random reference from store
randomRefFromStore :: NameSpace -> Store -> Maybe (State StdGen Reference)
randomRefFromStore ns s = randomRef $ filter isRef $ flattenStore $ subStoreAtNameSpace ns s

-- return substore at specified namespace
subStoreAtNameSpace :: NameSpace -> Store -> Store
subStoreAtNameSpace ns s = maybe (Store []) store (sfFind (s,ns))
	where
		store (SubStore s) = s
		store _ = Store []

-- flatten the store into a list of (reference,value) pairs
flattenStore :: Store -> [(Reference,StoreValue)]
flattenStore (Store ivs) = flattenStore' (Reference []) ivs
flattenStore' _ [] = []
flattenStore' r ((i,StoreValue v):ss) = (r,(StoreValue v)) : (flattenStore' r ss)
flattenStore' r ((i,(SubStore (Store ivs))):ss) = 
	(r,(SubStore (Store ivs))) : ( (flattenStore' (r |+| (Reference [i])) ivs) ++ (flattenStore' r ss) )

-- true if store item is a block reference (substore)
isBlock :: (Reference,StoreValue) -> Bool
isBlock (_,SubStore _) = True
isBlock _ = False

-- true if store item has reference with more than one identifier component
isRef :: (Reference,StoreValue) -> Bool
isRef ((Reference is),_) = (length is) > 1

-- pick random reference from list of store items
randomRef :: [(Reference,StoreValue)] -> Maybe (State StdGen Reference)
randomRef vs =
	if (numRefs==0) then Nothing else Just (randomRef' vs)
	where
		numRefs = length vs
		randomRef' vs = do
			n <- randomV
			let (r,s) = vs !! (n `mod` numRefs)
			return r
