-- | Main entry point to the application.
module Main where

-- import Debug.Trace (trace, traceShow)
import Include
import LabGen

import Control.Monad (forM_)
import Control.Monad.List (guard)
import Control.Monad.IO.Class (liftIO)

import Data.Array
import Data.List (lookup)
import UI.NCurses

--              (Up    Down  Left  Right) Glyph
gridGlyphs :: [([Bool], Glyph)]
gridGlyphs = [
    ([False, True , False, True ], glyphCornerUL),
    ([True , False, False, True ], glyphCornerLL),
    ([False, True , True , False], glyphCornerUR),
    ([True , False, True , False], glyphCornerLR),
    ([True , True , False, True ], glyphTeeL),
    ([True , True , True , False], glyphTeeR),
    ([True , False, True , True ], glyphTeeB),
    ([False, True , True , True ], glyphTeeT),
    ([False, False, True , True ], glyphLineH),
    ([True , True , False, False], glyphLineV),
    ([True , True , True , True ], glyphPlus),
    ([True , False, False, False], glyphLineV),
    ([False, True , False, False], glyphLineV),
    ([False, False, True , False], glyphLineH),
    ([False, False, False, True ], glyphLineH)]

gridGlyph :: LabGrid -> LabIdx -> Char
gridGlyph grd idx = 
    let d = do
            d <- dirs
            let idx' = idx .+ newIdx d
            
            return $ (inBounds' grd idx') && not (grd ! idx')
     
     in case d `lookup` gridGlyphs of
            Just g  -> glyphCharacter g
            Nothing -> glyphCharacter glyphBlock
        
printLab :: Window -> LabGrid -> Curses ()
printLab w grd = do
        let (x1, y1) = gridSize grd
            idxChr idx = if (grd ! idx) then ' ' else gridGlyph grd idx
            rowStr y = map idxChr [ (x,y) | x <- [0..x1] ]
        
        updateWindow w $ setAttribute AttributeBold True
        
        forM_ [0..y1] $ \y -> do
            updateWindow w $ do
                moveCursor (toInteger $ y) 0
                drawString $ rowStr y
        updateWindow w $ do
            setAttribute AttributeBold False
            moveCursor 0 0

-- | The main entry point.
main :: IO ()
main = runCurses $ do
        (rc, cc) <- screenSize 
        
        let opts = LabOpts (fromIntegral cc - 5, fromIntegral rc - 5) 0.6
        
        setEcho False
        -- _ <- setCursorMode CursorInvisible --doesn't work on windoze

        do
            let (he, wi) = (5, 40)
                (hr, hc) = (rc `div` 2 - he `div` 2,
                            cc `div` 2 - wi `div` 2)
            w <- newWindow he wi hr hc
            updateWindow w $ do
                drawBox Nothing Nothing
                moveCursor (he `div` 2) 5
                drawString "Preparing maze"
        
        render
    
        grd <- liftIO $ buildGrid opts
        
        w <- defaultWindow
        printLab w grd
        
        red <- newColorID ColorRed ColorBlack 1
        
        updateWindow w $ do
            moveCursor 1 1
            setAttribute AttributeBlink True
            setColor red
            drawString "?"
            moveCursor 1 1
            setColor $ defaultColorID
        
        render
        waitFor w $ \ev -> ev `elem` (map EventCharacter ['q', 'Q'])    


waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
