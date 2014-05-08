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
    compile SF source
------------------------------------------------------------------------------}

compile :: Bool -> String -> String -> IO (String)
compile isComparing sourcePath destPath = do
	let fmt = if (isComparing) then SFpFormat else NativeFormat
	-- parse it & evaluate if the parse succeeds
	source <- readFile sourcePath
	storeOrError <- parseSF sourcePath source
	let result = case (storeOrError) of
		Left e -> Left $ err (EPARSEFAIL e) $ fmt
		Right body -> case (evalSF body) of
			Left error -> Left $ ( error $ fmt ) ++ "\n"
			Right store -> Right $ ( renderStore store ) ++ "\n" where
				renderStore = if (isComparing) then renderCompactJSON else renderJSON
	-- of we are comparing outputs, put the error message in the file
	-- otherwise, print it to the stderr
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
	let dstPath = jsonPath srcPath opts
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
