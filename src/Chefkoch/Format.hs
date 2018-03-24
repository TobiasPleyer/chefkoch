{-# LANGUAGE OverloadedStrings #-}


module Chefkoch.Format where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml

import Chefkoch.DataTypes
import Chefkoch.DataFunctions


formatterMap = [
    ("raw" , rawFormatter )
  , ("json", jsonFormatter)
  , ("yaml", yamlFormatter)
  ]


rawFormatter :: [Recipe] -> B.ByteString
rawFormatter = BC.pack . show


jsonFormatter :: [Recipe] -> B.ByteString
jsonFormatter = BL.toStrict . Json.encode


yamlFormatter :: [Recipe] -> B.ByteString
yamlFormatter = Yaml.encode
