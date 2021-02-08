{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Applicative
import           Data.Char
import           Numeric
import           System.Exit

data Input = Input
  { inputLoc :: Int
  , inputStr :: String
  } deriving (Show, Eq)

-- | Pull the first character of the input if there is one still input
inputUncons :: Input                  -- input to check
            -> Maybe (Char, Input)
inputUncons (Input _ [])       = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

