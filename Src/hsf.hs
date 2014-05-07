{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix (takeBaseName)

import HSF.Parser
import HSF.Eval
import HSF.Utils
import HSF.Options
import HSF.RunScalaVersion

{------------------------------------------------------------------------------
    compile using the scala or haskell compiler
------------------------------------------------------------------------------}

-- TODO: **** split this function into smaller ones

compile :: Bool -> String -> String -> IO (String)
compile isComparing sourcePath destPath = do
	-- parse it & evaluate if the parse succeeds
	source <- readFile sourcePath
	-- the () here is the initial state  
	-- notice the runParserT                 
	storeOrError <- parseSF sourcePath source
	let result = case (storeOrError) of
		Left e -> Left $ err EPARSEFAIL [ (show e) ] $ isComparing
		Right body -> case (evalSF body) of
			Left error -> Left $ ( error $ isComparing ) ++ "\n"
			Right store -> Right $ ( renderStore store ) ++ "\n" where
				renderStore = if (isComparing) then renderCompactJSON else renderJSON
	-- of we are comparing outputs, put the error message in the file
	-- otherwise, print it to the stderr
	-- TODO: *** the sourcepath here is the top-level file 
	-- *** if we are inside a #include, we really want to print the included file ?
	case (result) of
		Left e -> if (isComparing)
			then writeFile destPath e
			else hPutStrLn stderr ( "** " ++ sourcePath ++ "\n" ++ e )
		Right json -> if (destPath == "-")
			then putStr json
			else writeFile destPath json
	-- return the results or the error message
	case (result) of
		Left e -> return e
		Right json -> return json


{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do
    (args, files) <- getArgs >>= parseOptions
    mapM_ (process args) files

process :: [OptionFlag] -> String -> IO ()
process opts srcPath = do
	let dstPath = if ((getOutputPath opts) == "-")
		then (++) "-"
		else getJsonPath srcPath opts
	if (compareOptionPresent opts) then do
		haskellResult <- compile True srcPath (dstPath "-1")
		scalaResult <- runSfParser srcPath (dstPath "-2") opts
		if (haskellResult == scalaResult)
			then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			else putStr ( "** match failed: " ++ (takeBaseName srcPath) ++ "\n"
				++ "Haskell: " ++ haskellResult ++ "Scala:   " ++ scalaResult )
	else do
		compile False srcPath (dstPath "")
		return ()
	return ()
