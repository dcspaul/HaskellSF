{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Error Messages
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Errors
	( Error(..)
	, errorString, parseError
	) where

import Data.String.Utils (rstrip)
import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (Message(..), errorMessages)

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

data Error

	= ESYSFAIL String
	| EPARSEFAIL String
	| EPARENTNOTSTORE String
	| ENOPARENT String
	| EREPLACEROOTSTORE
	| ENOPROTO String
	| EPROTONOTSTORE String
	| ENOLR String
	| EASSIGN String
	| EREFNOTOBJ String
	| ENOSPEC
	| ESPEC String
	
	| S_EPROTONOTSTORE String
	| S_ENOLR String
	| S_EASSIGN String
	| S_ENOSPEC String
	| S_EPARSEFAIL String

-- printable strings for error messages

errorString :: Error -> String
errorString code = case (code) of
	
	ESYSFAIL s -> "command failed: " ++ s
	EPARSEFAIL s -> s
	EPARENTNOTSTORE s -> "parent not a store [error 1]: " ++ s
	ENOPARENT s -> "reference has no parent [error 2]: " ++ s
	EREPLACEROOTSTORE -> "attempt to replace root store [error 3]"
	ENOPROTO s -> "can't resolve prototype [error 4]: " ++ s
	EPROTONOTSTORE s -> "prototype is not a store [error 4]: " ++ s
	ENOLR s -> "can't resolve link value [error 5]: " ++ s
	EASSIGN s -> "can't resolve reference [error 6]: " ++ s
	EREFNOTOBJ s -> "reference not an object [error 6]: " ++ s
	ENOSPEC -> "no sfConfig at top level of specification [error 7]"
	ESPEC s -> "sfConfig cannot be a basic value [error 7]: " ++ s
	
	S_EPROTONOTSTORE s -> s
	S_ENOLR s -> s
	S_EASSIGN s -> s
	S_ENOSPEC s -> s
	S_EPARSEFAIL s -> s
	
-- equality of error messages
-- this is used to compare the error messages from different implementations.
-- messages are generally counted as equal if they are the same type, 
-- regardless of the details of the accompanying text

instance Eq Error where
	
	ESYSFAIL a == ESYSFAIL b = True
	EPARSEFAIL a == EPARSEFAIL b = True
	EPARENTNOTSTORE a == EPARENTNOTSTORE b = True
	ENOPARENT a == ENOPARENT b = True
	EREPLACEROOTSTORE == EREPLACEROOTSTORE = True
	ENOPROTO a == ENOPROTO b = True
	EPROTONOTSTORE a == EPROTONOTSTORE b = True
	ENOLR a == ENOLR b = True
	EASSIGN a == EASSIGN b = True
	EREFNOTOBJ a == EREFNOTOBJ b = True
	ENOSPEC == ENOSPEC = True
	ESPEC a == ESPEC b = True
	
	S_EPROTONOTSTORE a == EPROTONOTSTORE b = True
	S_ENOLR a == ENOLR b = True
	S_EASSIGN a == EASSIGN b = True
	S_ENOSPEC a == ENOSPEC = True
	S_EPARSEFAIL a == EPARSEFAIL b = True

	_ == _ = False

{------------------------------------------------------------------------------
    parser error messages
------------------------------------------------------------------------------}

-- this function formats error messages from the parser
-- and wraps them in an EPARSEFAIL

parseError :: ParseError -> Error
parseError e =
	EPARSEFAIL ( "parse error at " ++ f ++ " (line " ++ (show l) ++
		", column " ++ (show c) ++ ")\n" ++ msg )

	where
		(f, l, c) = breakPos e
		msg = let msgs = errorMessages e in
			if (isFail msgs)
				then failMessage msgs
				else rstrip $ unlines $ tail $ lines $ show e

		breakPos e = (f, l, c) where
			pos = errorPos e
			f = sourceName pos
			l = sourceLine pos
			c = sourceColumn pos

		isFailMessage (Message m) = True
		isFailMessage _ = False

		isFail msgList = any isFailMessage msgList

		failMessage msgList = s
			where (Message s) = head $ filter isFailMessage msgList 
