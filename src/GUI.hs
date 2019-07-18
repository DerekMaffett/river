module GUI where

import Reflex.Dom
import qualified Config

getConfigFromGUI forceRebuild = do
  mainWidget $ do
    evClick <- button "Click Me!"
    isVisible <- foldDyn (\_ b -> not b) False evClick
    textWithDynamicVisibility isVisible
    return ()
  return $ Config.Config {}

textWithDynamicVisibility isVisible = do
  let dynAttr = ffor isVisible
                 (\case
                   True -> ("style" =: "")
                   False -> ("style" =: "display: none;"))

  elDynAttr "div" dynAttr $
    text "Click the button again to make me disappear!"
