module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool true) = "true"
renderJValue (JBool false) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where   pairs [] = ""
            pairs ps = intercalate ", " (map renderPair ps)
            renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ value a ++ "]"
    where   value [] = ""   
            values vs = intercalate "," (map renderJValue vs)

putValue :: JValue -> IO ()
putValue = putStrLn . renderJValue