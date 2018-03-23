module Chefkoch.Format where


import Chefkoch.DataTypes
import Chefkoch.DataFunctions


formatterMap = [
    ("raw" , rawFormatter)
  , ("yaml", yamlFormatter)
  ]


rawFormatter :: Recipe -> String
rawFormatter = show


yamlFormatter :: Recipe -> String
yamlFormatter = undefined
