import Lib
import Data.Maybe
import Data.List

accessToken :: String
accessToken = "7542~kMBJqLyi52mSMiP3YyivwzN0SHrHw6GUnraUAhyRTBAl3KcLlwBoStGAYa0z2cta"

main :: IO ()
main = do
  courses <- getCourses accessToken
  let avg = transform courses
  print avg

type Average = Float

transform :: [Course] -> Average
transform = average . mapMaybe grade

average :: [Grade] -> Average
average gs = sum gs / genericLength gs
