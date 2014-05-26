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

inventProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	fB <- inventBody bp $ (r,s)
	result <- inventProtoList ps $ (ns, r, (store fB))
	return ( result { node =  (BodyProto (node fB)):(node result) } )

inventProtoList ((RefProto (Reference [Identifier "?ref"])):ps) = \(ns,r,s) -> do
	let rp = inventProtoRef ns s
	s' <- sfInherit(s,ns,rp,r)
	result <- inventProtoList ps $ (ns, r, s')
	return ( result { node = (RefProto rp):(node result) } )

inventProtoList ((RefProto r):_) =
	error ( "impossible! generated refproto not ?ref: " ++ (show r) )

inventProtoList ([]) = \(ns,r,s) -> Right ( Result { store = s, node = [] } )

inventValue :: Value -> (NameSpace,Reference,Store) -> ResultOrError Value

inventValue (BasicValue bv) = \(ns,r,s) -> do
	s' <- sfBind(s, r, StoreValue bv)
	return ( Result { store = s', node = BasicValue bv } )

inventValue (LinkValue (Reference [Identifier "?ref"])) = \(ns,r,s) -> do
	let lr = inventLinkRef ns s
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> error ( "impossible! cannot resolve generated link: " ++ (show lr) )
		Just (n,v) -> Right (n,v)
	s' <- sfBind(s, r, v')
	return ( Result { store = s', node = LinkValue lr } )

inventValue (LinkValue r) =
	error ( "impossible! generated linkvalue not ?ref: " ++ (show r) )

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	result <- inventProtoList ps $ (ns,r,s')
	return ( result { node = (ProtoValue (node result)) } )

inventAssignment :: Assignment -> (NameSpace,Store) -> ResultOrError Assignment

inventAssignment (Assignment (Reference [Identifier "?ref"]) v) = inventAssignment' inventLHSRef v

inventAssignment (Assignment (Reference [Identifier "?id"]) v) = inventAssignment' inventLHSId v

inventAssignment (Assignment r v) = inventAssignment' (\ns s -> r) v

inventAssignment' :: (NameSpace -> Store -> Reference) -> Value -> (NameSpace,Store) -> ResultOrError Assignment

inventAssignment' f v = \(ns,s) -> do
	let r = f ns s
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

inventProtoRef ns s = (Reference [Identifier "fooPROTO"])
inventLinkRef ns s = (Reference [Identifier "fooLink"])
inventLHSRef ns s = (Reference [Identifier "fooREF"])
inventLHSId ns s = (Reference [Identifier "fooID"])
