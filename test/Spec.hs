module Main where

import qualified QC.Planets
import qualified QC.Observations
import System.Exit
    
main :: IO ()
main = do
    -- add test runners into the array for each module
    good <- and <$> sequence [ QC.Planets.runTests
                             , QC.Observations.runTests ]
    if good
        then exitSuccess
        else exitFailure
