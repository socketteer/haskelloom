{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace


data World = World {
    token :: String,
    amplitude :: Float,
    children :: Maybe [World]
}

drawBlock :: World -> Picture 
drawBlock World{..} =
    scale amplitude amplitude $ Pictures [color white(line [(0, 0), (0, -1), (-1, -1), (-1, 0), (0, 0)]),
              foldMap drawBlocks children,
              color white(translate (-0.8) (-0.5) $ scale 0.0005 0.0005 $ Text token)]

drawBlocks :: [World] -> Picture
drawBlocks = foldr pushBlock Blank 

pushBlock :: World -> Picture -> Picture
pushBlock w@World{..} siblings = drawBlock w <> translate 0 ((-1) * amplitude) siblings

generatePicture :: World -> IO Picture
generatePicture w = return (scale 1500 1500 $ translate 0.5 0.5 $ drawBlock w)

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) = return . traceShow (x, y)
eventHandler _ = return

simulationWindow :: Display
simulationWindow = InWindow "Multiverse" (1500, 1500) (100, 100)

w = World "root" 1 (Just [World "child1" 0.5 Nothing, World "child2" 0.2 
    (Just [World "child4" 0.5 (Just [World "child5" 0.5 Nothing])]), 
    World "child3" 0.1 Nothing])

main :: IO ()
main = do 
    playIO simulationWindow black 0 w generatePicture eventHandler (\_ world -> return world)
    
    
    --simulationWindow black (World "root" 1 (Just [World "child1" 0.5 Nothing, World "child2" 0.2 
    --    (Just [World "child4" 0.5 Nothing]), World "child3" 0.1 Nothing])) generatePicture 
    --    eventHandler (\(Controller _ controllerModifyViewPort) -> controllerModifyViewPort (\viewport -> return viewport))