{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( Error(..)
	, errorString
	, indentBlock, indentMsg
	) where

import Data.String.Utils (replace, rstrip)
import Data.List (intercalate)
import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (Message(..), errorMessages)

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

data Error
	= EPARSEFAIL ParseError
	| ESFPARSEFAIL String
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

errorString :: Error -> String
errorString code = case (code) of
	ESFPARSEFAIL s -> "sfparser fail: " ++ s
	EPARSEFAIL e -> parseError e
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

instance Eq Error where
	EPARSEFAIL a == EPARSEFAIL b = (parseError a) == (parseError b)
	ESFPARSEFAIL a == ESFPARSEFAIL b = (a==b)
	EPARENTNOTSTORE a == EPARENTNOTSTORE b = (a==b)
	ENOPARENT a == ENOPARENT b = (a==b)
	EREPLACEROOTSTORE == EREPLACEROOTSTORE = True
	ENOPROTO a == ENOPROTO b = (a==b)
	EPROTONOTSTORE a == EPROTONOTSTORE b = (a==b)
	ENOLR a == ENOLR b = (a==b)
	EASSIGN a == EASSIGN b = (a==b)
	EREFNOTOBJ a == EREFNOTOBJ b = (a==b)
	ENOSPEC == ENOSPEC = True
	ESPEC a == ESPEC b = (a==b)

-- these functions format the error messages from the parser

parseError :: ParseError -> String
parseError e =
	"parse error at " ++ f ++ " (line " ++ (show l) ++ ", column " ++ (show c) ++ ")\n" ++ msg
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
    output formatting
------------------------------------------------------------------------------}

tabString :: Int -> String
tabString n = foldl1 (++) (replicate n " ")

indentMsgBy :: String -> String -> String
indentMsgBy ts text
	| length text == 0		= text
	| otherwise				= (replace "\n" ("\n" ++ ts) text)

indentBlockBy :: String -> String -> String
indentBlockBy ts text
	| length text == 0		= text
	| otherwise				= "\n" ++ ts ++ (replace "\n" ("\n" ++ ts) text) ++ "\n"

indentBlock :: String -> String
indentBlock = indentBlockBy (tabString 2)

indentMsg :: String -> String
indentMsg = indentMsgBy (tabString 3)
