module GUI where

import Reflex.Dom
import Data.FileEmbed
import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Text as T
import           Control.Monad.IO.Class
import qualified Config
import qualified Data.Map as Map
import qualified Data.Aeson as A

getConfigFromGUI forceRebuild = do
  files <- Config.getConfigFiles
  mainWidgetWithCss css $ bodyElement files
  return $ Config.Config {}


css = $(embedFile "src/init.css")

createDropdownOptions = zipWith (\index (val, displayVal) -> (index, val, displayVal)) ([0..] :: [Integer])

getReflexDropdownOptions :: [(Integer, a, T.Text)] -> Map.Map Integer T.Text
getReflexDropdownOptions = Map.fromList . fmap (\(index, val, displayVal) -> (index, displayVal))

getValueFromIndex options index = fromJust . fmap (\(_, val, _) -> val) . find (\(i, _, _) -> i == index) $ options
getIndexFromValue options value = fromJust . fmap (\(i, _, _) -> i) . find (\(_, val, _) -> val == value) $ options

dropdownValue options ddropdown = getValueFromIndex options <$> _dropdown_value ddropdown

bodyElement :: MonadWidget t m => (Maybe A.Object, Maybe A.Object) -> m ()
bodyElement configFiles = do
    let 
      getField = getFieldFromConfigFiles configFiles


      repoManagerOptions = createDropdownOptions [(Config.GithubManager, "Github"), (Config.BitbucketManager, "Bitbucket")]
      repoManagerType = getField Config.GithubManager Config.repoManagerTypeF
                                       
    elClass "section" "section" $ do
      elClass "div" "container columns" $ do
        elClass "div" "column is-half is-offset-one-quarter" $ do
          elClass "h1" "title" $ text "River project settings"
          ddi <- selectField "Repo Manager" repoManagerType repoManagerOptions
          ti <- textField "Working Branch" (getFieldFromConfigFiles configFiles "" Config.workingBranchF)
          eClick <- button "submit"
          let eFormSubmit = tagPromptlyDyn (dropdownValue repoManagerOptions ddi) eClick
          performEvent_ (ffor eFormSubmit $ \val -> liftIO . putStrLn . show $ val)
    return ()

getFieldFromConfigFiles :: (A.FromJSON a) => (Maybe A.Object, Maybe A.Object) -> a -> Config.DataPathSuggestion -> a
getFieldFromConfigFiles files defaultValue dataPathSuggestion = 
  fromRight defaultValue $ Config.parseConfig files dataPathSuggestion

selectField :: (Eq a, MonadWidget t m) => T.Text -> a -> [(Integer, a, T.Text)] -> m (Dropdown t Integer)
selectField label initialValue options = 
  elClass "div" "field" $ do
    elClass "label" "label" $ do
      text label
      elClass "div" "control is-expanded" $ do
        elClass "div" "select is-fullwidth" $ do
          dropdown (getIndexFromValue options initialValue) (constDyn . getReflexDropdownOptions $ options) $ def

textField :: MonadWidget t m => T.Text -> T.Text -> m (TextInput t)
textField label initialValue = 
  elClass "div" "field" $ do
    elClass "label" "label" $ do
      text label
      elClass "div" "control" $ do
        textInput $ def 
          & textInputConfig_initialValue .~ initialValue
          & attributes .~ constDyn ("class" =: "input")
