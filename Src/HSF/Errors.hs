{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Error Messages
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Errors
	( Error(..), ErrorMessage
	, errorString, errorCode, parseError, outputOrError
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
	| ENOSPEC
	| ESPEC String

-- printable strings for error messages

class ErrorMessage a where
	errorString :: a -> String 
	errorCode :: a -> String 
	
instance ErrorMessage Error where

	errorString (ESYSFAIL s) = "command failed: " ++ s
	errorString (EPARSEFAIL s) = s
	errorString (EPARENTNOTSTORE s) = "parent not a store [error 1]: " ++ s
	errorString (ENOPARENT s) = "reference has no parent [error 2]: " ++ s
	errorString EREPLACEROOTSTORE = "attempt to replace root store [error 3]"
	errorString (ENOPROTO s) = "can't resolve prototype [error 4]: " ++ s
	errorString (EPROTONOTSTORE s) = "prototype is not a store [error 4]: " ++ s
	errorString (ENOLR s) = "can't resolve link value [error 5]: " ++ s
	errorString ENOSPEC = "no sfConfig at top level of specification [error 7]"
	
	errorCode (ESYSFAIL s) = "sys fail"
	errorCode (EPARSEFAIL s) = "parse fail"
	errorCode (EPARENTNOTSTORE s) = "err1"
	errorCode (ENOPARENT s) = "err2"
	errorCode EREPLACEROOTSTORE = "err3"
	errorCode (ENOPROTO s) = "err4"
	errorCode (EPROTONOTSTORE s) = "err4"
	errorCode (ENOLR s) = "err5"
	errorCode ENOSPEC = "err7"
	errorCode (ESPEC s) = "err7"

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

{------------------------------------------------------------------------------
    string result of compilation
------------------------------------------------------------------------------}

outputOrError :: ErrorMessage e => (Either e String) -> String
outputOrError r =
	case r of
		Left e -> errorString e
		Right s -> s
