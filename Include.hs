module Include (Direction (..), LabIdx, LabGrid, LabOpts (..), dirs,grid, gridSize, invDir, newIdx, (.+), inBounds, inBounds') where

import Data.Array

data Direction = DUp | DDown | DLeft | DRight | DNone
    deriving (Eq, Ord, Show)
    
dirs = [DUp, DDown, DLeft, DRight]

type LabIdx = (Int, Int)
type LabGrid = Array LabIdx Bool


grid :: LabIdx -> LabGrid
grid size = fmap (const False) $ array ((0,0), size) []

gridSize :: LabGrid -> LabIdx
gridSize = snd . bounds


invDir :: Direction -> Direction
invDir DUp    = DDown
invDir DDown  = DUp
invDir DLeft  = DRight
invDir DRight = DLeft
invDir DNone  = DNone

newIdx :: Direction -> LabIdx
newIdx DUp    = ( 0, -1)
newIdx DDown  = ( 0,  1)
newIdx DLeft  = (-1,  0)
newIdx DRight = ( 1,  0)
newIdx DNone  = ( 0,  0)

(.+) :: LabIdx -> LabIdx -> LabIdx
(x1, y1) .+ (x2,y2) = (x1+x2, y1+y2)


inBounds :: LabGrid -> LabIdx -> Bool
inBounds grd (x, y) = let (xm, ym) = gridSize grd
                      in (x > 0) && (y > 0) && (x < xm) && (y < ym)

inBounds' :: LabGrid -> LabIdx -> Bool
inBounds' grd (x,y) = let (xm, ym) = gridSize grd
                      in (x >= 0) && (y >= 0) && (x <= xm) && (y <= ym)

data LabOpts = LabOpts {
    getGridSize :: (Int, Int),
    getTwiceProp :: Double}
