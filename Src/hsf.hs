{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix (takeBaseName)
import Control.Monad (void)

import HSF.Parser
import HSF.Eval
import HSF.Utils
import HSF.Errors
import HSF.Options
import HSF.RunScalaVersion
import HSF.QuickCheck

{------------------------------------------------------------------------------
    compile SF source
------------------------------------------------------------------------------}

compile :: Opts -> String -> IO (Either Error String)
compile opts srcPath = do

		source <- readFile srcPath
		storeOrError <- parseSF srcPath source
		case (storeOrError) of
			Left e -> return (Left (parseError e))
			Right parseTree -> return (eval parseTree)
	where
		
		eval :: Body -> (Either Error String)
		eval parseTree =
			case (evalSF parseTree) of
				Left e -> Left e
				Right store -> renderStore store

		renderStore :: Store -> (Either Error String)
		renderStore store = Right s
			where s = case (format opts) of
				JSON -> ( renderJSON store )
				CompactJSON -> ( renderCompactJSON store )

compileAndSave :: Opts -> String -> IO()
compileAndSave opts srcPath = do

	result <- compile opts srcPath
	case (result) of
		Left e -> hPutStrLn stderr ( "** " ++ srcPath ++ "\n   " ++ (indentMsg (errorString e)) )
		Right s -> if (dstPath == "-") then putStrLn s else writeFile dstPath (s++"\n")
	where dstPath = jsonPath srcPath opts ""

compileAndCompare :: Opts -> String -> IO()
compileAndCompare opts srcPath = do

		haskellResult <- compile (opts { format=CompactJSON } ) srcPath
		scalaResult <- runSfParser opts srcPath
	
		if (haskellResult == scalaResult)
			then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			else putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"
				++ "Haskell: " ++ (resultString haskellResult) ++ "\n"
				++ "Scala:   " ++ (resultString scalaResult) ))
	where

		resultString :: (Either Error String) -> String
		resultString r =
			case r of
				Left e -> errorString e
				Right s -> s

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do

	(opts, files) <- getArgs >>= parseOptions
	do if (isChecking opts) then check else return ()
	mapM_ (processFile opts) files
	
processFile :: Opts -> String -> IO ()
processFile opts srcPath = do
	if (isComparing opts)
		then compileAndCompare opts srcPath
		else compileAndSave opts srcPath
