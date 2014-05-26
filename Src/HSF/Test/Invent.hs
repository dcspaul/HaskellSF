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

-- cabal install Safe
import Safe (initSafe)

{------------------------------------------------------------------------------
    evaluation functions
------------------------------------------------------------------------------}

data Result a = Result
	{ store :: Store
	, node :: a
	}

type ResultOrError a = Either Error (Result a)

inventProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> ResultOrError [Prototype]

-- invent a reference to a random prototype 
-- if we can't find one, generate an empty body prototype
inventProtoList ((RefProto (Reference [Identifier "?ref"])):ps) = \(ns,r,s) -> do
	let rp = inventProtoRef ns s
	inventProtoList (rp:ps) $ (ns,r,s)
	
-- a body prototype
inventProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	fB <- inventBody bp $ (r,s)
	result <- inventProtoList ps $ (ns, r, (store fB))
	return ( result { node =  (BodyProto (node fB)):(node result) } )

-- a reference to a prototype
inventProtoList ((RefProto rp):ps) = \(ns,r,s) -> do
	s' <- sfInherit(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ([]) = \(ns,r,s) -> Right ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> ResultOrError Value

inventValue (BasicValue bv) = \(ns,r,s) -> do
	s' <- sfBind(s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	let v' = inventLinkRef ns s
	inventValue v' $ (ns,r,s)

inventValue (LinkValue lr) = \(ns,r,s) -> do
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> Right (n,v)
	s' <- sfBind(s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	result <- inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

inventAssignment :: Assignment -> (NameSpace,Store) -> ResultOrError Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = \(ns,s) -> do
	let r = inventLHSRef ns s
	result <- inventValue v $ (ns, (ns |+| r), s)
	return ( result { node = Assignment r (node result) } )
	
inventAssignment (Assignment (Reference [Identifier "?id"]) v) = \(ns,s) -> do
	let r = inventLHSId
	result <- inventValue v $ (ns, (ns |+| r), s)
	return ( result { node = Assignment r (node result) } )
	
inventAssignment (Assignment r v) = \(ns,s) -> do
	result <- inventValue v $ (ns, (ns |+| r), s)
	return ( result { node = Assignment r (node result) } )

inventBody :: Body -> (NameSpace,Store) -> ResultOrError Body

inventBody (Body (a:b)) = \(ns,s) -> do
	fA <- inventAssignment a $ (ns,s)
	result <- inventBody (Body b) $ (ns,(store fA))
	let (Body as) = node result
	return ( result { node = Body ((node fA):as) } )

inventBody (Body []) = \(ns,s) -> Right ( Result { store = s, node = (Body []) } )
	

inventSF :: SFConfig -> Either Error SFConfig
 
inventSF (SFConfig as) = do
	result <- inventBody (Body as) $ (Reference [], Store [])
	let (Body as') = node result
	return (SFConfig as')

----------------

inventLHSId :: Reference
inventLHSId = (Reference [Identifier "fooID"])
-- invent a random ID

inventLHSRef :: NameSpace -> Store -> Reference
inventLHSRef ns s = inventLHSId
-- inventLHSRef ns s = (Reference [Identifier "fooREF"])
-- look in the store for a random reference
-- if you can't find anything, use inventLHSId

inventProtoRef :: NameSpace -> Store -> Prototype
inventProtoRef ns s = (BodyProto (Body []))
-- inventProtoRef ns s = (RefProto (Reference [Identifier "fooPROTO"]))
-- look in the store for a random reference to a prototype
-- if you can't find one, then generate an empty body

inventLinkRef :: NameSpace -> Store -> Value
inventLinkRef ns s = (BasicValue (StringValue "frobble!"))
-- inventLinkRef ns s = (LinkValue (Reference [Identifier "fooLink"]))
-- look in the store for a random reference to a prototype or a value
-- if you can't find one, generate some arbitrary value
	





