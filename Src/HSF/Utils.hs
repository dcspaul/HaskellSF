{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( indentBlock, indentMsg
	) where

import Data.String.Utils (replace, rstrip)

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
