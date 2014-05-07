{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( ErrorCode(..)
	, err
	, indentBlock
	) where

import Data.String.Utils (replace)

{------------------------------------------------------------------------------
    error messages
------------------------------------------------------------------------------}

-- err returns a function which gives the appropriate version of the message
-- depending on whether we are doing a comparison with sfparser or not

data ErrorCode = EPARSEFAIL | EPARENTNOTSTORE | ENOPARENT | EREPLACEROOTSTORE |
	ENOPROTO | EPROTONOTSTORE | ENOLR | EASSIGN | EREFNOTOBJ | ENOSPEC | ESPEC

err :: ErrorCode -> [String] -> Bool -> String
err code args = \isComparing -> case (code) of
	EPARSEFAIL	-> if (isComparing)
		then (id a) ++ "\n"
		else "parse failed: " ++ (id a) ++ "\n"
	EPARENTNOTSTORE -> "parent not a store [error 1]: " ++ a
	ENOPARENT -> "reference has no parent [error 2]: " ++ a
	EREPLACEROOTSTORE -> "attempt to replace root store [error 3]"
	ENOPROTO -> "can't resolve prototype [error 4]: " ++ a
	EPROTONOTSTORE -> "prototype is not a store [error 4]: " ++ a
	ENOLR -> if (isComparing)
		then "[err5] cannot find link reference " ++ a
		else "can't resolve link value [error 5]: " ++ a
	EASSIGN -> if (isComparing)
		then "[err6] prefix of " ++ a ++ " is not a component"
		else "can't resolve reference [error 6]: " ++ a
	EREFNOTOBJ -> "reference not an object [error 6]: " ++ a
	ENOSPEC -> if (isComparing)
		then "[err7] sfConfig is not exist or a component"
		else "no sfConfig at top level of specification [error 7]"
	ESPEC -> "sfConfig cannot be a basic value [error 7]: " ++ a
	where a = args!!0

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
