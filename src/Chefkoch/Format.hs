{-# LANGUAGE OverloadedStrings #-}


module Chefkoch.Format where


import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml

import Chefkoch.DataTypes
import Chefkoch.DataFunctions


formatterMap = [
    ("raw" , rawFormatter )
  , ("json", jsonFormatter)
--  , ("yaml", yamlFormatter)
  ]


rawFormatter :: [Recipe] -> B.ByteString
rawFormatter = C.pack . show


jsonFormatter :: [Recipe] -> B.ByteString
jsonFormatter = Json.encode


--yamlFormatter :: Recipe -> String
--yamlFormatter = Yaml.encode
