import           Control.Monad      (mapM_)
import           Data.List          (intercalate, sort)
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath    ((</>))
import           System.IO
import           System.Process     (callCommand)

templateDir = "somepath\\templates"  -- path of templates

main :: IO ()
main = do
    args <- getArgs
    langs <- getAvalibleLanguages
    case parseArgs args langs of
        Left "help" -> showHelpMessage
        Left "error" -> showHelpMessage
        Left "list" -> putStrLn $ "Avalible languages are:\n" ++ intercalate "\n" langs
        Right projInfo -> createProject projInfo


showHelpMessage :: IO ()
showHelpMessage = putStrLn "ProjGen\nuseage:\nprojgen LANGUAGE PROJECT_NAME [PACKAGES]\nprojgen list"

parseArgs :: [String] -> [String] -> Either String (String, String, [String])   -- (language name, project name, [packages])
parseArgs ("help":_) _ = Left "help"
parseArgs ("list":_) _ = Left "list"
parseArgs args langs | length args < 2 = Left "error"
                     | head args `notElem` langs = Left "list"
                     | otherwise = Right (head args, (head.tail) args, drop 2 args)

getAvalibleLanguages :: IO [String]
getAvalibleLanguages = do
    dirs <- getDirectoryContents templateDir
    return $ sort (filter (`notElem` [".", ".."]) dirs)

createProject :: (String, String, [String]) -> IO ()
createProject (lang,proj,packages) = do
    copyDir templatePath projectPath
    gitInit projectPath
        where templatePath = templateDir </> lang
              projectPath = "." </> proj

copyDir :: FilePath -> FilePath -> IO ()
copyDir src des = do
    fileExists <- doesFileExist src
    if fileExists
    then copyFile src des
    else do
        createDirectory des
        files <- getDirectoryContents src
        mapM_ (\target -> copyDir (src </> target) (des </> target)) (filter (not.(`elem` [".", "..", "projgen-config"])) files)

gitInit :: FilePath -> IO ()
gitInit projectPath = do
    setCurrentDirectory projectPath
    callCommand "git init"
    callCommand "git add ."
    callCommand "git commit -m \"init\""
    return ()
