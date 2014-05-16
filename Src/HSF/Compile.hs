{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Compile
	( compile
	, compileAndSave
	, compileAndCompare
	, resultString
	) where

import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix (takeBaseName)

import HSF.Parser
import HSF.Eval
import HSF.Utils
import HSF.Errors
import HSF.Options
import HSF.Test.RunScalaVersion

{------------------------------------------------------------------------------
    compile SF source from file
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

{------------------------------------------------------------------------------
    compile from file and save to file
------------------------------------------------------------------------------}

compileAndSave :: Opts -> String -> IO ()
compileAndSave opts srcPath = do

	result <- compile opts srcPath
	case (result) of
		Left e -> hPutStrLn stderr ( "** " ++ srcPath ++ "\n   " ++ (indentMsg (errorString e)) )
		Right s -> if (dstPath == "-") then putStrLn s else writeFile dstPath (s++"\n")
	where dstPath = jsonPath srcPath opts ""

{------------------------------------------------------------------------------
    compile from file with multiple compilers & compare result
------------------------------------------------------------------------------}

compileAndCompare :: Opts -> String -> IO ()
compileAndCompare opts srcPath = do

	haskellResult <- compile (opts { format=CompactJSON } ) srcPath
	scalaResult <- runSfParser opts srcPath

	if (matchSfParser haskellResult scalaResult)
		then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
		else putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"
			++ "Haskell: " ++ (resultString haskellResult) ++ "\n"
			++ "Scala:   " ++ (resultString scalaResult) ))

{------------------------------------------------------------------------------
    string result of compilation
------------------------------------------------------------------------------}

resultString :: ErrorMessage e => (Either e String) -> String
resultString r =
	case r of
		Left e -> errorString e
		Right s -> s
