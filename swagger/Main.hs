{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Lens               ((&), (.~), (?~))
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Swagger
import           Lib                        (api)
import           Servant.Swagger            (toSwagger)
import           System.Directory           (getCurrentDirectory)
import           System.FilePath.Posix      ((</>))

apiSwagger :: Swagger
apiSwagger = toSwagger api
    & info.title   .~ "Calcetthon API"
    & info.version .~ "0.1.0"
    & info.description ?~ "The API for our foosball scores"
    & info.license ?~ ("All Rights Reserved")

main :: IO ()
main = do
    currentDirectory <- getCurrentDirectory
    BL8.writeFile (currentDirectory </> "swagger" </> "swagger.json") (encodePretty apiSwagger)
