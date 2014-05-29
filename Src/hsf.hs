{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import HSF.Parser
import HSF.Store
import HSF.Eval
import HSF.Utils
import HSF.Errors
import HSF.Options
import HSF.Test.QuickCheck
import HSF.Test.RunScalaVersion
import HSF.Test.RunOCamlVersion
import HSF.Test.RunHPVersion

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do

	-- command line args
	(opts, files) <- getArgs >>= parseOptions

	-- quickcheck ?
	do
		case (checkWith opts) of
			NoCompare -> return ()
			ScalaCompiler -> doCompare opts compile compareWithScala
			OCamlCompiler -> doCompare opts compile compareWithOCaml
			HPCompiler -> doCompare opts compile compareWithHP

	-- process each file on the command line
	mapM_ (processFile opts) files
	where
		processFile opts srcPath = do
			case (compareWith opts) of
				NoCompare -> compileAndSave opts srcPath
				ScalaCompiler -> compareWithScala opts compile srcPath
				OCamlCompiler -> compareWithOCaml opts compile srcPath
				HPCompiler -> compareWithHP opts compile srcPath

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
		
		eval :: SFConfig -> (Either Error String)
		eval parseTree =
			case (evalSF parseTree) of
				Left e -> Left e
				Right store -> renderStore store

		renderStore :: Store -> (Either Error String)
		renderStore store = Right s
			where s = case (format opts) of
				JSON -> ( renderJSON store )
				CompactJSON -> ( renderCompactJSON store )
				HPSF -> ( renderHPSF store )

{------------------------------------------------------------------------------
    compile and save to file
------------------------------------------------------------------------------}

compileAndSave :: Opts -> String -> IO (Bool)
compileAndSave opts srcPath = do

	result <- compile opts srcPath
	case (result) of
		Left e -> do
			hPutStrLn stderr ( "** " ++ srcPath ++ "\n   " ++ (indentMsg (errorString e)) )
			return False
		Right s -> do
			if (dstPath == "-") then putStrLn s else writeFile dstPath (s++"\n")
			return True
	where dstPath = jsonPath srcPath opts ""
