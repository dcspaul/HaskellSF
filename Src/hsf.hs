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

compile :: ErrorFormat -> String -> String -> IO (String)
compile fmt sourcePath destPath = do
	
		source <- readFile sourcePath
		storeOrError <- parseSF sourcePath source
		case (storeOrError) of
			Left e -> parseFail e
			Right parseTree -> eval parseTree
	where
		
		isComparing = (fmt == SFpFormat)
		
		parseFail e = compileFail (err (EPARSEFAIL e) fmt)

		eval parseTree =
			case (evalSF parseTree) of
				Left e -> evalFail e
				Right store -> output store

		evalFail e = compileFail ((e fmt) ++ "\n")

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
	
		if (compareOptionPresent opts)
			then compileAndCompare
			else compileOnly
	where

		dstPath = jsonPath srcPath opts

		compileOnly = do compile NativeFormat srcPath (dstPath "") ; return ()

		compileAndCompare = do
			haskellResult <- compile SFpFormat srcPath (dstPath "-1")
			scalaResult <- runSfParser srcPath (dstPath "-2") opts
			if (haskellResult == scalaResult)
				then matchOK
				else matchFail haskellResult scalaResult
			
		matchOK = putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )

		matchFail h s = putStr ( "** match failed: " ++ (takeBaseName srcPath) ++ "\n"
			++ "Haskell: " ++ h ++ "Scala:   " ++ s )





