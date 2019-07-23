module GUI where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import qualified Data.Text as T
import           Control.Monad.IO.Class
import qualified Config
import qualified Data.Map as Map

getConfigFromGUI forceRebuild = do
  files <- Config.getConfigFiles
  let maybeWorkingBranch = fromEither $ Config.parseConfig files Config.workingBranchF
  mainWidgetWithCss css $ bodyElement maybeWorkingBranch
  return $ Config.Config {}


css = $(embedFile "src/init.css")

fromEither = either (const Nothing) Just
toString = T.pack . fromMaybe "jk"

bodyElement :: MonadWidget t m => Maybe String -> m ()
bodyElement wb = do
    elClass "section" "section" $ do
      elClass "div" "container columns" $ do
        elClass "div" "column is-half is-offset-one-quarter" $ do
          elClass "h1" "title" $ text "River project settings"
          ddi <- elClass "div" "field" $ do
            elClass "label" "label" $ do
              text "Repo Manager"
              elClass "div" "control is-expanded" $ do
                elClass "div" "select is-fullwidth" $ do
                  let options = constDyn $ Map.fromList[(1, "github"), (2, "bitbucket")]
                  dropdown 1 options $ def
                    & attributes .~ constDyn ("class" =: "input")

          ti <- elClass "div" "field" $ do
            elClass "label" "label" $ do
              text "Working Branch"
              elClass "div" "control" $ do
                textInput $ def 
                  & textInputConfig_initialValue .~ (toString wb)
                  & attributes .~ constDyn ("class" =: "input")
          eClick <- button "submit"
          let eFormSubmit = tagPromptlyDyn (value ti) eClick
          performEvent_ (ffor eFormSubmit $ \val -> liftIO . putStrLn . T.unpack $ val)
    return ()
