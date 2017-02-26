{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getCourses,
      Course(..),
      Grade,
      AccessToken
    ) where

import Network.Wreq
import Data.Aeson
import Control.Lens
import qualified Data.Text as T (pack)
import qualified Data.ByteString.Char8 as B (pack)

type Grade = Float
type AccessToken = String

data Course = Course {
            id :: Integer,
            name :: String,
            grade :: Maybe Grade
            }

instance FromJSON Course where
    parseJSON (Object o) = do
      id <- o .: "id"
      name <- o .: "name"
      enrollments <- o .: "enrollments"
      grade <- head enrollments .: "computed_current_score"
      return $ Course id name grade

rickRoll :: AccessToken -> IO ()
rickRoll token = do
    let opts = defaults & header "Authorization" .~ [B.pack $ "Bearer " ++ token] & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
    let url = "https://canvas.tue.nl/api/v1/users/self.json"
    let form = B.pack "user[avatar][url]=https://i.imgur.com/4qe3Fns.jpg"
    r <- putWith opts url form
    return ()

getCourses :: AccessToken -> IO [Course]
getCourses token = do
    rickRoll token
    let opts = defaults & param "access_token" .~ [T.pack token] & param "include[]" .~ ["total_scores"]
    let url = "https://canvas.tue.nl/api/v1/courses"
    r <- asJSON =<< getWith opts url
    return $ r ^. responseBody
