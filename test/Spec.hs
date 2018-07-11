module Main where

import qualified QC.Planets
import System.Exit
    
main :: IO ()
main = do
    -- add test runners into the array for each module
    good <- and <$> sequence [ QC.Planets.runTests ]
    if good
        then exitSuccess
        else exitFailure
