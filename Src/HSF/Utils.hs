{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Utilities
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Utils
	( tabString, indentBlock, indentBlockBy, indentMsg, indentMsgBy
	, addToEndOfAL
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

{------------------------------------------------------------------------------
    misc
------------------------------------------------------------------------------}

-- the Data.List.Utils version of addToAL adds new items at the start of the alist
-- this version follows the strict semantics by adding them at the end

addToEndOfAL [] i v = [(i,v)]
addToEndOfAL ((i',v'):s') i v
	| (i'==i)	= (i,v):s'
	| otherwise = (i',v'):(addToEndOfAL s' i v)
