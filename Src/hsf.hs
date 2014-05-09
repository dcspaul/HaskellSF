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
	
		source <- readFile sourcePath
		storeOrError <- parseSF sourcePath source
		case (storeOrError) of
			Left e -> parseFail e
			Right parseTree -> eval parseTree
	where
		
		fmt = if (isComparing) then SFpFormat else NativeFormat
		
		parseFail e = compileFail (err (EPARSEFAIL e) $ fmt)

		eval parseTree =
			case (evalSF parseTree) of
				Left e -> evalFail e
				Right store -> output store

		evalFail e = compileFail (( e $ fmt ) ++ "\n")

		output store = do
			let renderStore = if (isComparing) then renderCompactJSON else renderJSON
			let json = ( renderStore store ) ++ "\n"
			if (destPath == "-") then putStr json else writeFile destPath json
			return json
		
		compileFail msg = do
			if (isComparing)
				then writeFile destPath msg
				else hPutStrLn stderr ( "** " ++ sourcePath ++ "\n" ++ msg )	
			return msg

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
