module Main where

import Lib
import Data.Maybe
import Data.List

type Average = Float

accessToken :: String
accessToken = "7542~kMBJqLyi52mSMiP3YyivwzN0SHrHw6GUnraUAhyRTBAl3KcLlwBoStGAYa0z2cta"

main :: IO ()
main = do
    putStrLn "Enter access token:"

  -- INPUT
    token <- getLine
    input <- getCourses token

  -- TRANSFORM
    let result = transform input

    -- OUTPUT
    list input
    display result

transform :: [Course] -> Average
transform = average . mapMaybe grade

average :: [Float] -> Float
average gs = sum gs / genericLength gs

display :: Average -> IO ()
display avg = do
    putStrLn $ "Average grade: " ++ show avg ++ "/100"
    advice avg

list :: [Course] -> IO ()
list = mapM_ print

advice :: Average -> IO ()
advice avg
    | avg < 55 = putStrLn "You'd better step it up buddy"
    | avg > 65 = putStrLn "Way to go dude!"
    | otherwise = putStrLn "You're living on the edge"
