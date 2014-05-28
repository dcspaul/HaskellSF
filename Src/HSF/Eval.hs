{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Evaluator
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Eval
	( evalSF
	) where

import HSF.Parser
import HSF.Store
import HSF.Errors
import HSF.Utils

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
		Nothing -> Left ( ENOLR (render lr) )
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

evalAssignment (Assignment r v) = \(ns,s) -> do
	evalValue v $ (ns, (ns |+| r), s)

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
	
evalSF :: SFConfig -> StoreOrError

evalSF (SFConfig as) = do
	fB <- evalBody (Body as) $ (Reference [], Store [])
	case (sfFind(fB,Reference [Identifier "sfConfig"])) of
		Nothing -> Left ENOSPEC
		Just (StoreValue bv) -> Left ( ESPEC (render bv) )
		Just (SubStore s) -> return s
