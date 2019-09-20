module BugTracker
    ( trackResolution
    )
where

import           Data.HashMap.Strict           as HM
import           System.Exit
import           Config
import qualified Control.Monad.Reader          as Reader
import qualified Logger
import qualified Types
import qualified Data.Aeson                    as A
import qualified System.Directory              as Dir

trackResolution :: Types.Issue -> Program ()
trackResolution issue = do
    Config { bugCategories } <- ask
    resolution               <- Logger.queryWithSuggestions
        "Bug cause (tab for suggestions): "
        bugCategories
    Reader.liftIO $ persistResolution (Types.key issue) resolution

persistResolution key resolution = do
    bugResolutions <- readFromFileOrInitData
    A.encodeFile ".river-bugs.json" bugResolutions
  where
    readFromFileOrInitData = do
        bugsFileExists <- Dir.doesFileExist ".river-bugs.json"
        if bugsFileExists
            then do
                result <- A.eitherDecodeFileStrict' ".river-bugs.json"
                case result of
                    Left  errorMsg -> die $ show errorMsg
                    Right bugs     -> return $ HM.insert key resolution bugs
            else return $ HM.singleton key resolution
