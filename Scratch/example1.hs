-- http://book.realworldhaskell.org/read/using-parsec.html

import Text.ParserCombinators.Parsec

-- functions: many noneof char

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}

-- input is a sequence of Char
-- st is "user supplied state" (not used yet)? see http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#GenParser
-- output is a list of lists of strings [[String]]

-- the "Parser" type is shorthand for GenParser Char ()
-- csvFile :: GenParser Char st [[String]]
-- the type is often omitted for so many small functions & the compiler works it out
csvFile :: Parser [[String]]

-- the use of a "do" block implies that Parsec is a "monadic library" which defines its own monad
-- what is the "monadic payload"? the parse state of some kind?

-- "many" calls the given function (line) repeatedly to parse individual lines (here) and returns a list of them
-- then we check for eof (library function?) and return the list of lines

-- here's how the monad stuff works ...
-- a) the "line" is a (Char) Parser returning a [String]
-- b) the "many line" is (presumably) a (Char) parser returning a [[String]] 
-- c) the <- picks the inside out of the parser (presumably) a [[String]]
-- d) the return wraps the [[String]] to give you parser returning a [[String]]

csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result

-- Build up a list of cells.  Try to parse the first cell, then figure out what ends the cell.
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line

-- the "char ','" swallows the comma
-- no "do" block here - I guess this is because we have only one "statement"
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or EOL
-- the EOL handling here seems a bit messy - "\n" occurs in multiel places & woudl it work on Windows?
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

main = do { result <- parseFromFile csvFile "/Users/paul/Work/Playground/Haskell/sfParser/test.csv"
          ; case (result) of
             Left err  -> print err
             Right strings  -> print strings
          }
