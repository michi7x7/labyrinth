module LabGen (buildGrid) where

import Include

import Data.Array ((!), (//))
import Data.List (delete, permutations)
import Control.Monad.List (guard)
import Control.Monad.State (State, get, put, evalState)
import System.Random (RandomGen, random, randomR, newStdGen)

possiblePaths :: LabGrid -> LabIdx -> [Direction]
possiblePaths grd idx = do
    dir <- dirs
    let idx' = idx .+ newIdx dir
    guard $ inBounds grd idx'
    guard $ (grd ! idx') == False
    guard $ isPath idx' dir
    return dir
    where isPath idx dir =
            let ns = do
                    let dir' = invDir dir
                    d1 <- [DLeft, DRight, DNone]
                    d2 <- [DUp, DDown, DNone]
                    
                    guard $ d1 /= dir'
                    guard $ d2 /= dir'
                    guard $ d1 /= d2
                    
                    let
                        di   = newIdx d1 .+ newIdx d2
                        idx' = idx .+ di
                    guard $ inBounds grd idx'
                    guard $ (grd ! idx') == True
                    return True
            in null ns

rndPick :: (RandomGen g) => [a] -> State g a
rndPick a = do
    gen <- get
    let (i, gen') = randomR (0, length a - 1) gen
    put gen'
    return $ a !! i

rndFlag :: (RandomGen g) => Double -> State g Bool
rndFlag t = do
    gen <- get
    let (r, gen') = random gen
    put gen'
    return $ r < t

buildGrid' :: (RandomGen g) => LabIdx -> LabGrid -> State g LabGrid
buildGrid' idx grd = let
    dirs = possiblePaths grd idx
    in do
        if null dirs then
            return grd
        else do
            dir <- rndPick dirs
            twice <- rndFlag 0.60
            end   <- rndFlag 0.01
            
            let idx' = idx .+ newIdx dir
                grd' = grd // [(idx', True)]
                n = if twice then 2 else if end then 0 else 1
            
            if end then
                return grd'
            else if twice then
                return grd' >>= buildGrid' idx' >>= buildGrid' idx'
            else
                return grd' >>= buildGrid' idx'


buildGrid :: LabOpts -> IO LabGrid
buildGrid opts = do
    rgen <- newStdGen
    let size = getGridSize opts
        rbot = size .+ (-1, -1)
        grd  = grid size // [(rbot, True)]
    return $ evalState (buildGrid' rbot grd) rgen
