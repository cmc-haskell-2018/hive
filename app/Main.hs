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

-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | gameEnding game /= Nothing = game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ shiftGame $ 
        makeMove (getCell mouse) game    -- фишка уже взята
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | gameEnding game /= Nothing = game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = game    -- фишка еще не взята, отменять нечего
  | otherwise = putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = game

-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id