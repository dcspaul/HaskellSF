{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Command line options
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Options
	( OptionFlag
	, parseOptions
	, getOutputPath
	, getJsonPath
	, getSfParserPath
	, compareOptionPresent
	) where


import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt(getOpt,OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, (</>))
import System.Exit (exitWith,ExitCode(..))
import System.Environment (lookupEnv)

{------------------------------------------------------------------------------
    option handling
------------------------------------------------------------------------------}

-- output directory arg:
-- with no slash, it is treated as a subdirectory of the source directory
-- you can use ".." in this to refer to directories above the source
-- with a slash it is treated as an absolute pathname
-- the default is "" which is the same directory as the source
-- you can use "-" for stdout

data OptionFlag = Output String | Compare | SfParser String deriving(Show,Eq)

options :: [OptDescr OptionFlag]
options =
	[ Option ['o'] ["output"]	(ReqArg Output "DIR")		"directory for json output"
	, Option ['c'] ["compare"]	(NoArg Compare)				"compare with output of Scala compiler"
	, Option ['s'] ["sfparser"]	(ReqArg SfParser "FILE")	"location of sfparser"
	]
 
parseOptions :: [String] -> IO ([OptionFlag], [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(args,fs,[]) -> do
		return (args,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

getOutputPath :: [OptionFlag] -> String
getOutputPath [] = ""
getOutputPath ((Output d):_) = d
getOutputPath (_:rest) = getOutputPath rest

getSfParserPath :: [OptionFlag] -> IO (String)
getSfParserPath fs = do
	let arg = sfParserArg fs
	sfParserEnv <- lookupEnv "SFPARSER"
	case (arg) of
		Just f -> return f
		Nothing -> case (sfParserEnv) of
			Just s -> return s
			Nothing -> return "sfparser"

sfParserArg :: [OptionFlag] -> Maybe String
sfParserArg [] = Nothing
sfParserArg ((SfParser f):_) = Just f
sfParserArg (_:rest) = sfParserArg rest

compareOptionPresent :: [OptionFlag] -> Bool
compareOptionPresent opts = (Compare `elem` opts)

-- TODO: *** document this!

getJsonPath :: String -> [OptionFlag] -> String -> String
getJsonPath srcPath opts ext =
	((takeDirectory srcPath) </> (getOutputPath opts) </>
		(addExtension ((dropExtension (takeFileName srcPath)) ++ ext) ".json"))
