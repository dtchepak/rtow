module Main where

import qualified RayTracingOneWeekend as RT
import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  createDirectoryIfMissing
    False -- do not create parent dirs
    "output"
  LBS.writeFile "output/example2_fromWp.ppm" $
    RT.pack RT.example2_fromWp
  LBS.writeFile "output/example2_fromWp2.ppm" $
    RT.pack RT.example2_fromWp'
  LBS.writeFile "output/example2_1.ppm" $
    RT.pack RT.example2_1
  putStrLn "Files written to output/"


