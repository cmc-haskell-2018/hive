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
    fps     = 5      -- кол-во кадров в секунду

-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | gamePlayer game == Black = game     -- если сейчас ходит бот, мы ничего не можем сделать
  | gameEnding game /= Nothing = game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = takePiece coord game    -- фишка еще не взята
  | otherwise = makeMove coord game    -- фишка уже взята
          where
            coord = getCell mouse
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | gamePlayer game == Black = game     -- если сейчас ходит бот, мы ничего не можем сделать
  | gameEnding game /= Nothing = game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = game    -- фишка еще не взята, отменять нечего
  | otherwise = putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = game

-- | Обновление игры. Вызывает функцию бота
updateGame :: Float -> Game -> Game
updateGame _ game
  | gamePlayer game == Beige = game
  | gameEnding game /= Nothing = game
  | gameMovable game /= Nothing = game
  | otherwise = makeMove put $ takePiece take game
    where
      (take, put) = bot game
      bot = futurePlusPrinciplesBot 2 [getCloser 1, getCloser 2, bringIn Ant, bringIn Beetle, bringIn Mosquito, bringIn LadyBug, bringIn Hopper]
