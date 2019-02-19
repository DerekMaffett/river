module Git
    ( openBranch
    , setOrigin
    , getCurrentBranch
    , getIssueKeyFromBranch
    )
where

import qualified Logger                        as L
import           Process
import           Config
import           Data.Char
import           Data.List
import           Text.Parsec
import           Control.Monad


setOrigin :: Program ()
setOrigin = do
    Config { repoOrg, repoName } <- ask
    originExists                 <- includesOrigin <$> (runProcess "git remote")
    when originExists $ runProcess' $ "git remote remove river-origin"
    runProcess'
        $  "git remote add river-origin git@bitbucket.org:"
        <> repoOrg
        <> "/"
        <> repoName
        <> ".git"
    where includesOrigin = any (== "river-origin") . lines

openBranch :: String -> Program ()
openBranch branchName = do
    Config { workingBranch } <- ask
    L.logNotice $ "Checking out " <> workingBranch <> "..."
    runProcess' $ "git checkout " <> workingBranch
    L.logNotice $ "Pulling new changes on " <> workingBranch <> "..."
    runProcess' "git pull"
    L.logNotice "Creating branch..."
    runProcess' $ "git checkout -b " <> branchName
    L.logNotice "Pushing branch to remote..."
    runProcess' $ "git push -u river-origin " <> branchName

getCurrentBranch :: Program String
getCurrentBranch = trim <$> runProcess "git rev-parse --abbrev-ref HEAD"
    where trim = dropWhile isSpace . dropWhileEnd isSpace

getIssueKeyFromBranch :: String -> Either ParseError String
getIssueKeyFromBranch branchName = parse parser "" branchName
  where
    parser = do
        many1 $ noneOf "/"
        char '/'
        prefix      <- many1 letter
        dash        <- oneOf "-"
        issueNumber <- many1 digit
        return $ prefix <> [dash] <> issueNumber
