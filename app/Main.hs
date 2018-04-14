module Main where

import Hive
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  game <- initGame
  play display bgColor fps game drawGame handleGame updateGame
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 0      -- кол-во кадров в секунду
