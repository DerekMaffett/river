module Utils
    ( trim
    )
where

import           Data.Char
import           Data.List

trim = dropWhile isSpace . dropWhileEnd isSpace
