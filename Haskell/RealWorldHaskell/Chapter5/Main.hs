module Main (main) where

import SimpleJSON

main = print (JArray [JNull,JString "abc",JNumber 123.4,JBool False,JObject [("this",JNull),("that",JBool True)]])