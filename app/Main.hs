module Main where

import Lib
import Data.Maybe
import Data.List

type Average = Float

accessToken :: String
accessToken = "7542~kMBJqLyi52mSMiP3YyivwzN0SHrHw6GUnraUAhyRTBAl3KcLlwBoStGAYa0z2cta"

main :: IO ()
main = do
  -- INPUT
    readToken <- getToken
    let token = case readToken of
          Just t -> t
          Nothing -> accessToken

    courses <- getCourses token

  -- TRANSFORM
    let avg = transform courses

    -- OUTPUT
    list courses
    display avg

getToken :: IO (Maybe String)
getToken = do
    putStrLn "Enter access token:"
    line <- getLine
    return $ if line == ""
      then Nothing
      else Just line


transform :: [Course] -> Average
transform = average . mapMaybe grade

average :: [Float] -> Float
average gs = sum gs / genericLength gs

display :: Average -> IO ()
display avg = do
    putStrLn ("Average grade: " ++ show avg ++ "/100")
    putStrLn (advice avg)

list :: [Course] -> IO ()
list = mapM_ print

advice :: Average -> String
advice avg
    | avg < 55 = "You'd better step it up buddy"
    | avg > 65 = "Way to go dude!"
    | otherwise = "You're living on the edge"
