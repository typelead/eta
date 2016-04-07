module Java.META.Spec where

import Data.Char (toLower)

import Java.META.Types

class MetaSpec s where
  loadFirstSection :: Section -> s

  loadOtherSection :: s -> Section -> s
  loadOtherSection s _ = s

  storeMeta :: s -> META

loadSpec :: (MetaSpec s) => META -> s
loadSpec [] = error "Cannot load empty metadata"
loadSpec (s:ss) =
  let x = loadFirstSection s
  in  foldl loadOtherSection x ss

lookupList :: String -> Maybe String -> [(String, String)]
lookupList _ Nothing = []
lookupList name (Just val) = [(name, val)]

bool2string :: Bool -> String
bool2string True = "true"
bool2string False = "false"

string2bool :: String -> Bool
string2bool s
  | map toLower s == "true" = True
  | otherwise = False

