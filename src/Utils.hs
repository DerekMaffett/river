module Utils
    ( trim
    , setAtPath
    , Writeable(..)
    )
where

import           Data.Char
import           Data.List
import           Data.Text (Text)
import           Data.Aeson                    as A
import qualified Data.HashMap.Strict           as HM

trim = dropWhile isSpace . dropWhileEnd isSpace

data Writeable = forall a . ToJSON a => MkWriteable a
instance ToJSON Writeable where
  toJSON (MkWriteable val) = toJSON val

setAtPath :: [Text] -> A.Object -> Writeable -> A.Object
setAtPath pathSegments fileObject content = setValue pathSegments fileObject
  where
    setValue :: [Text] -> A.Object -> A.Object
    setValue segments obj = case segments of
        x : y : xs -> HM.alter (recurseOnObject (y:xs)) x obj
        x     : _  -> HM.insert x (toJSON content) obj
        []         -> obj
    recurseOnObject remainingSegments maybeValue = case maybeValue of
        Nothing           -> Just $ Object $ setValue remainingSegments HM.empty
        Just (Object obj) -> Just $ Object $ setValue remainingSegments obj
