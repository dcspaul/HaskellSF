{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( ErrorCode(..), ErrorFormat(..)
	, err
	, indentBlock
	) where

import Data.String.Utils (replace, rstrip)
import Data.List (intercalate)
import Text.Parsec (ParseError, errorPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (Message(..), errorMessages)

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

-- err returns a function which gives the appropriate version of the message
-- depending on whether we are doing a comparison with sfparser or not

data ErrorFormat =  NativeFormat | SFpFormat deriving(Eq)

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
		SFpFormat -> (sfpParseError e) ++ "\n"
		NativeFormat -> (nativeParseError e) ++ "\n"
	EPARENTNOTSTORE s -> "parent not a store [error 1]: " ++ s
	ENOPARENT s -> "reference has no parent [error 2]: " ++ s
	EREPLACEROOTSTORE -> "attempt to replace root store [error 3]"
	ENOPROTO s -> "can't resolve prototype [error 4]: " ++ s
	EPROTONOTSTORE s -> case fmt of
		SFpFormat -> "[err4] invalid prototype reference: " ++ s
		NativeFormat -> "prototype is not a store [error 4]: " ++ s
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
-- if we are comparing with sfParser, we make some attempt to produce a similar format

nativeParseError :: ParseError -> String
nativeParseError e =
	"parse error at " ++ f ++ " (line " ++ (show l) ++ ", column " ++ (show c) ++ ")\n" ++ msg
	where
		(f, l, c) = breakPos e
		msg = let msgs = errorMessages e in
			if (isFail msgs)
				then failMessage msgs
				else rstrip $ unlines $ tail $ lines $ show e

sfpParseError :: ParseError -> String
sfpParseError e =
	let msgs = errorMessages e in
		if (isFail msgs)
			then failMessage msgs
			else "invalid statement at " ++ (show l) ++ "." ++ (show c)
			where (f, l, c) = breakPos e

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

indentBlockBy :: String -> String -> String
indentBlockBy ts text
	| length text == 0		= text
	| otherwise				= "\n" ++ ts ++ (replace "\n" ("\n" ++ ts) text) ++ "\n"

indentBlock :: String -> String
indentBlock = indentBlockBy (tabString 2)
