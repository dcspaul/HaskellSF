{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( ErrorCode(..), ErrorFormat(..)
	, err
	, indentBlock
	) where

import Data.String.Utils (replace)
import Data.List (intercalate)
import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (Message(..), errorMessages)

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

-- err returns a function which gives the appropriate version of the message
-- depending on whether we are doing a comparison with sfparser or not

data ErrorFormat =  NativeFormat | SFpFormat

data ErrorCode
	= EPARSEFAIL ParseError
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

err :: ErrorCode -> ErrorFormat -> String
err code = \fmt -> case (code) of
	EPARSEFAIL e -> case fmt of
		SFpFormat -> (nativeParseError e) ++ "\n"
		NativeFormat -> (sfpParseError e) ++ "\n"
	EPARENTNOTSTORE s -> "parent not a store [error 1]: " ++ s
	ENOPARENT s -> "reference has no parent [error 2]: " ++ s
	EREPLACEROOTSTORE -> "attempt to replace root store [error 3]"
	ENOPROTO s -> "can't resolve prototype [error 4]: " ++ s
	EPROTONOTSTORE s -> "prototype is not a store [error 4]: " ++ s
	ENOLR s -> case fmt of
		SFpFormat -> "[err5] cannot find link reference " ++ s
		NativeFormat -> "can't resolve link value [error 5]: " ++ s
	EASSIGN s -> case fmt of
		SFpFormat -> "[err6] prefix of " ++ s ++ " is not a component"
		NativeFormat -> "can't resolve reference [error 6]: " ++ s
	EREFNOTOBJ s -> "reference not an object [error 6]: " ++ s
	ENOSPEC -> case fmt of
		SFpFormat -> "[err7] sfConfig is not exist or a component"
		NativeFormat -> "no sfConfig at top level of specification [error 7]"
	ESPEC s -> "sfConfig cannot be a basic value [error 7]: " ++ s

-- these functions format the error messages from the parser

nativeParseError :: ParseError -> String
nativeParseError e =
	"parse error at " ++ f ++ " (line " ++ (show l) ++ ", column " ++ (show c) ++ ")\n" ++ msg
	where
		pos = errorPos e
		f = sourceName pos
		l = sourceLine pos
		c = sourceColumn pos
		msg = errorMessage (errorMessages e)

sfpParseError :: ParseError -> String
sfpParseError e =
	"parse error at " ++ f ++ " (line " ++ (show l) ++ ", column " ++ (show c) ++ ")\n" ++ msg
	where
		pos = errorPos e
		f = sourceName pos
		l = sourceLine pos
		c = sourceColumn pos
		msg = errorMessage (errorMessages e)

isFailMessage (Message m) = True
isFailMessage _ = False

isFail msgList = any isFailMessage msgList

failMessage msgList = s
	where (Message s) = head $ filter isFailMessage msgList 

errorMessage :: [Message] -> String
errorMessage msgs =
	if (isFail msgs)
		then failMessage msgs
		else (intercalate "\n" (map msgString msgs))
		where msgString m = case m of
			SysUnExpect s -> "unexpected: " ++ s
			UnExpect s -> "unexpected: " ++ s
			Expect s -> "expected: " ++ s
			Message s -> "failed: " ++ s

{------------------------------------------------------------------------------
    output formatting
------------------------------------------------------------------------------}

tabString :: Int -> String
tabString n = foldl1 (++) (replicate n " ")

indentBlockBy :: String -> String -> String
indentBlockBy ts text
	| length text == 0		= text
	| otherwise				= "\n" ++ ts ++ (replace "\n" ("\n" ++ ts) text) ++ "\n"

indentBlock :: String -> String
indentBlock = indentBlockBy (tabString 2)
