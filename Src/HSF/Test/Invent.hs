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

inventProtoList :: [Prototype] -> (NameSpace,Reference,Store) -> Either Error (Store,[Prototype])

inventProtoList ((BodyProto bp):ps) = \(ns,r,s) -> do
	(fB,bp') <- inventBody bp $ (r,s)
	(s',ps') <- inventProtoList ps $ (ns, r, fB)
	return (s',(BodyProto bp'):ps')

inventProtoList ((RefProto rp):ps) = \(ns,r,s) -> do
	s' <- sfInherit(s,ns,rp,r)
	(s'',ps') <- inventProtoList ps $ (ns, r, s')
	return (s'',(RefProto rp):ps')

inventProtoList ([]) = \(ns,r,s) -> (Right (s,[]))

inventValue :: Value -> (NameSpace,Reference,Store) -> Either Error (Store,Value)

inventValue (BasicValue bv) = \(ns,r,s) -> do
	s' <- sfBind(s, r, StoreValue bv)
	return (s',(BasicValue bv))

inventValue (LinkValue lr) = \(ns,r,s) -> do
	(ns',v') <- case (sfResolv(s, ns, lr)) of
		Nothing -> Left ( ENOLR (render lr) )
		Just (n,v) -> Right (n,v)
	s' <- sfBind(s, r, v')
	return (s',(LinkValue lr))

inventValue (ProtoValue ps) = \(ns,r,s) -> do
	s' <- sfBind(s,r,SubStore (Store []))
	(s'',ps') <- inventProtoList ps $ (ns,r,s')
	return (s'',(ProtoValue ps'))

inventAssignment :: Assignment -> (NameSpace,Store) -> Either Error (Store,Assignment)

inventAssignment (Assignment r@(Reference [_]) v) = \(ns,s) -> do
	(s',v') <- inventValue v $ (ns, (ns |+| r), s)
	return (s',Assignment r v')

inventAssignment (Assignment r v) = \(ns,s) -> do
	case (sfResolv (s,ns,(sfPrefix r))) of
		Nothing -> Left ( EASSIGN (render r) )
		Just (_, StoreValue _) -> Left ( EREFNOTOBJ (render r) )
		Just (ns', _) -> do
			(s',v') <- inventValue v $ (ns, ns' |+| r, s)
			return (s',(Assignment r v'))

inventBody :: Body -> (NameSpace,Store) -> Either Error (Store,Body)

inventBody (Body (a:b)) = \(ns,s) -> do
	(fA,a') <- inventAssignment a $ (ns,s)
	(s',b') <- inventBody (Body b) $ (ns,fA)
	let (Body as) = b'
	return (s',Body (a':as))

inventBody (Body []) = \(ns,s) -> (Right (s,(Body [])))
	
inventSF :: SFConfig -> Either Error SFConfig

inventSF (SFConfig as) = do
	(_,(Body as')) <- inventBody (Body as) $ (Reference [], Store [])
	return (SFConfig as')
