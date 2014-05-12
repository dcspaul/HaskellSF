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
import HSF.Options
import HSF.RunScalaVersion
import HSF.QuickCheck


{------------------------------------------------------------------------------
    compile SF source
------------------------------------------------------------------------------}

compile :: [OptionFlag] -> String -> IO (Either Error String)
compile opts srcPath = do

		source <- readFile srcPath
		storeOrError <- parseSF srcPath source
		case (storeOrError) of
			Left e -> return (Left (EPARSEFAIL e))
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

compileAndSave :: [OptionFlag] -> String -> IO()
compileAndSave opts srcPath = do

	result <- compile opts srcPath
	case (result) of
		Left e -> hPutStrLn stderr ( "** " ++ srcPath ++ "\n   " ++ (indentMsg (errorString e)) )
		Right s -> if (dstPath == "-") then putStrLn s else writeFile dstPath (s++"\n")
	where dstPath = jsonPath srcPath opts ""

compileAndCompare :: [OptionFlag] -> String -> IO()
compileAndCompare opts srcPath = do

		haskellResult <- compile (setFormat opts CompactJSON) srcPath
		scalaResult <- runSfParser opts srcPath
	
		if (compareResult haskellResult scalaResult)
			then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			else putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"
				++ "Haskell: " ++ (resultString haskellResult) ++ "\n"
				++ "Scala:   " ++ (resultString scalaResult) ))
	where
	
		-- TODO: isn't this just an "Eq" ???
		compareResult :: (Either Error String) -> (Either Error String) -> Bool
		compareResult r1 r2 =
			case r1 of
				Left e1 -> case r2 of
					Left e2 -> e1 == e2
					Right s2 -> False
				Right s1 -> case r2 of
					Left e2 -> False
					Right s2 -> s1 == s2

		resultString :: (Either Error String) -> String
		resultString r =
			case r of
				Left e -> "[**] " ++ (errorString e)
				Right s -> "[OK] " ++ s

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do

	(opts, files) <- getArgs >>= parseOptions
	do if (checkOptionPresent opts) then check else return ()
	mapM_ (processFile opts) files
	
processFile :: [OptionFlag] -> String -> IO ()
processFile opts srcPath = do
	if (compareOptionPresent opts)
		then compileAndCompare opts srcPath
		else compileAndSave opts srcPath
