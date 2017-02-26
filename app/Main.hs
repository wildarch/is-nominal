module Main where

import Lib
import Data.Maybe
import Text.Printf

type Average = Float

accessToken :: String
accessToken = "7542~kMBJqLyi52mSMiP3YyivwzN0SHrHw6GUnraUAhyRTBAl3KcLlwBoStGAYa0z2cta"

main :: IO ()
main = do
    input <- getCourses $ accessToken
    list input
    let result = transform input
    display result

transform :: [Course] -> Average
transform = average . mapMaybe grade

average :: [Float] -> Float
average gs = sum gs / fromIntegral (length gs)

display :: Average -> IO ()
display avg = do
    putStrLn $ "Average grade: " ++ show avg ++ "/100"
    comment avg

list :: [Course] -> IO ()
list = mapM_ (\c -> printf "%s: %s\n" (name c) (show $ grade c))

comment :: Average -> IO ()
comment avg
    | avg < 55 = putStrLn "You'd better step it up buddy"
    | avg > 65 = putStrLn "Way to go dude!"
    | otherwise = putStrLn "You're safe for now"
