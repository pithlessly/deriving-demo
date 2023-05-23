{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH
import GHC.Generics -- (Generic)

data MyData1 = D1 Int String (Int, String) deriving (Show, Generic, GenericA)

data MyData2 = D2 Int String (Int, String) deriving (Show)
deriveA ''MyData2

main :: IO ()
main = print (a :: MyData2)
