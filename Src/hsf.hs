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

{--

* make some of these functions local to "compile"
* the two fail functions have a lot in common ?
* we need to import some types so that we can add the type signatures ?

--}


compile :: Bool -> String -> String -> IO (String)
compile isComparing sourcePath destPath = do
	let fmt = if (isComparing) then SFpFormat else NativeFormat
	-- parse it & evaluate if the parse succeeds
	source <- readFile sourcePath
	storeOrError <- parseSF sourcePath source
	case (storeOrError) of
		Left e -> parseFail isComparing e fmt destPath sourcePath
		Right body -> parseOK isComparing body fmt destPath sourcePath

-- parseFail :: Bool -> (ErrorFormat -> String) -> ErrorFormat -> String -> String -> String
parseFail isComparing e fmt destPath sourcePath = do
		let msg = (err (EPARSEFAIL e) $ fmt)
		if (isComparing)
			then writeFile destPath msg
			else hPutStrLn stderr ( "** " ++ sourcePath ++ "\n" ++ msg )
		return msg

parseOK :: Bool -> Body -> ErrorFormat -> String -> String -> IO String
parseOK isComparing parseTree fmt destPath sourcePath =
	case (evalSF parseTree) of
		Left e -> evalFail isComparing e fmt destPath sourcePath
		Right store -> evalOK isComparing store fmt destPath sourcePath

evalFail :: Bool -> (ErrorFormat -> String) -> ErrorFormat -> String -> String -> IO String
evalFail isComparing e fmt destPath sourcePath = do
		let msg = ( e $ fmt ) ++ "\n"
		if (isComparing)
			then writeFile destPath msg
			else hPutStrLn stderr ( "** " ++ sourcePath ++ "\n" ++ msg )
		return msg

-- evalOK :: Bool -> String -> ErrorFormat -> String -> String -> IO String
evalOK isComparing store fmt destPath sourcePath = do
	let renderStore = if (isComparing) then renderCompactJSON else renderJSON
	let result = ( renderStore store ) ++ "\n"
	if (destPath == "-") then putStr result else writeFile destPath result
	return result

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
