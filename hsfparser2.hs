import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith,ExitCode(..))
import System.Environment (getArgs)
import Data.List (nub)

main = do
    (args, files) <- getArgs >>= parseOptions
    mapM_ (process args) files

process :: [Flag] -> String -> IO ()
process as f = do
	putStrLn ( "FLAGS: " ++ (show as) )
	putStrLn ( "FILE: " ++ f )
	
data Flag = Verbose | Herry String | Paul String deriving(Show,Eq)

options :: [OptDescr Flag]
options =
	[ Option ['v'] ["verbose"]	(NoArg Verbose)			"chatty output"
	, Option ['h'] ["herry"]	(ReqArg Herry "DIR")	"compile using Herry's compiler"
	, Option ['p'] ["paul"]		(ReqArg Paul "DIR")		"compile using Paul's compiler"
	]
 
-- RequireOrder = no option processing after first non-option
parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(args,fs,[]) -> do
		return (args,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."
