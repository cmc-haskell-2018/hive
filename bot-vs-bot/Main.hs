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
    fps     = 1      -- кол-во кадров в секунду

-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame _ game = game

-- | Обновление игры. Вызывает функцию бота
updateGame :: Float -> Game -> Game
updateGame _ game
  | gameEnding game /= Nothing = game
  | gameMovable game /= Nothing = game
  | gamePlayer game == Beige = makeMove put1 $ takePiece take1 game    
  | otherwise = makeMove put2 $ takePiece take2 game
    where
      (take1, put1) = bot1 game
      bot1 = futurePlusPrinciplesBot 2 [getCloser 1, getCloser 2, bringIn Ant, bringIn Beetle, bringIn Hopper]
      (take2, put2) = bot2 game
      bot2 = futurePlusPrinciplesBot 0 []
