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
  | gameMovable game == Nothing = takePiece coord game    -- фишка еще не взята
  | otherwise = makeMove coord game    -- фишка уже взята
          where
            coord = getCell mouse
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | gameEnding game /= Nothing = game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = game    -- фишка еще не взята, отменять нечего
  | otherwise = putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = game

-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- | Определить клетку, в которую мы направляем мышкой
getCell :: Point -> Coord
getCell (xx, yy)
  | (mod (ii + jj)  2) == 0 = (ii, jj)
  | (y < ((x * (-3)) + j + (3 * i) -1)) && (y > ((3 * x) + j - (3 * i) +1)) = ((ii - 1), jj)
  | (y < (( 3 * x) + j - (3 * i) - 1)) && (y > (( (-3) * x) +j + (3 * i) +1)) = ((ii + 1), jj)
  | y > j  = (ii, (jj + 1))
  | otherwise = (ii, (jj - 1))
  where
    x =(xx / fromIntegral cellSizeX)
    y =(yy / fromIntegral cellSizeY)
    ii = round x
    jj = round y
    i = fromIntegral ii
    j = fromIntegral jj