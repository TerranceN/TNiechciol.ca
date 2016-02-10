module GitHash
( gitHash
, hashifyUrl
) where

import System.Environment as Env
import Data.String.Utils
import Data.Maybe

import HelperFunctions

gitHash :: IO String
gitHash = do
    env <- Env.getEnvironment
    return $ fromMaybe "" $ searchDict "GIT_HASH" env

hashifyUrl :: String -> String -> IO String
hashifyUrl url ending = do
  if (startswith "/" url)
    then do
      hash <- gitHash
      return $ (replace ending (hash++ending) url)
    else return url
