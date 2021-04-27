module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool b)   = if b then "true" else "false"
renderJValue JNull       = "null"

renderJValue (JObject o) = "{" ++ renderPairs o ++ "}"
    where renderPairs [] = ""
          renderPairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = k ++ " : " ++ renderJValue v

renderJValue (JArray vs) = "[" ++ renderJValues vs ++ "]"
    where renderJValues vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)    