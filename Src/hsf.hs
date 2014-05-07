{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Cmd(rawSystem)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith,ExitCode(..))
import System.Environment (getArgs,getExecutablePath,lookupEnv)
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, takeBaseName, (</>))
import GHC.IO.Exception
import System.Console.GetOpt

import HSF.Parser
import HSF.Eval
import HSF.Error

-- cabal install MissingH
import Data.List.Utils (addToAL)
-- import Data.String.Utils (join,replace)

-- cabal install Safe
-- import Safe (initSafe)

{------------------------------------------------------------------------------
    compile using the scala or haskell compiler
------------------------------------------------------------------------------}

sfParser :: String -> String -> String -> IO (String)
sfParser sourcePath destPath sfParserPath = do
	execPath <- getExecutablePath
	let scriptPath = (takeDirectory execPath) </> "runSfParser.sh"
 	exitCode <- rawSystem scriptPath [ sourcePath, destPath, sfParserPath  ]
	case (exitCode) of
		ExitSuccess -> readFile destPath
		ExitFailure code -> fail ("script failed: " ++ scriptPath ++
			 " " ++ sourcePath ++ " " ++ destPath ++ " " ++ sfParserPath )

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
    option handling
------------------------------------------------------------------------------}

-- output directory arg:
-- with no slash, it is treated as a subdirectory of the source directory
-- you can use ".." in this to refer to directories above the source
-- with a slash it is treated as an absolute pathname
-- the default is "" which is the same directory as the source
-- you can use "-" for stdout

data Flag = Output String | Compare | SfParser String deriving(Show,Eq)

options :: [OptDescr Flag]
options =
	[ Option ['o'] ["output"]	(ReqArg Output "DIR")		"directory for json output"
	, Option ['c'] ["compare"]	(NoArg Compare)				"compare with output of Scala compiler"
	, Option ['s'] ["sfparser"]	(ReqArg SfParser "FILE")	"location of sfparser"
	]
 
parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(args,fs,[]) -> do
		return (args,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

outputDir :: [Flag] -> String
outputDir [] = ""
outputDir ((Output d):_) = d
outputDir (_:rest) = outputDir rest

findSfParserPath :: [Flag] -> IO (String)
findSfParserPath fs = do
	let arg = sfParserArg fs
	sfParserEnv <- lookupEnv "SFPARSER"
	case (arg) of
		Just f -> return f
		Nothing -> case (sfParserEnv) of
			Just s -> return s
			Nothing -> return "sfparser"

sfParserArg :: [Flag] -> Maybe String
sfParserArg [] = Nothing
sfParserArg ((SfParser f):_) = Just f
sfParserArg (_:rest) = sfParserArg rest

-- TODO: *** document this!

jsonPath :: String -> String -> String -> String 
jsonPath srcPath relativeDir ext =
	((takeDirectory srcPath) </> relativeDir </>
		(addExtension ((dropExtension (takeFileName srcPath)) ++ ext) ".json"))

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do
    (args, files) <- getArgs >>= parseOptions
    mapM_ (process args) files

process :: [Flag] -> String -> IO ()
process opts srcPath = do
	let dstPath = if ((outputDir opts) == "-")
		then (++) "-"
		else jsonPath srcPath (outputDir opts)
	if (Compare `elem` opts) then do
		sfParserPath <- (findSfParserPath opts)
		haskellResult <- compile True srcPath (dstPath "-1")
		scalaResult <- sfParser srcPath (dstPath "-2") sfParserPath
		if (haskellResult == scalaResult)
			then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			else putStr ( "** match failed: " ++ (takeBaseName srcPath) ++ "\n"
				++ "Haskell: " ++ haskellResult ++ "Scala:   " ++ scalaResult )
	else do
		compile False srcPath (dstPath "")
		return ()
	return ()
