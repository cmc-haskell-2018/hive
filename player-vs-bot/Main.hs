module Main where

import Hive
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  game <- initGame
  playIO display bgColor fps game drawGame handleGame updateGame
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 5      -- кол-во кадров в секунду

-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> IO Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | gamePlayer game == Black = return $ game     -- если сейчас ходит бот, мы ничего не можем сделать
  | gameEnding game /= Nothing = return $ game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = return $ takePiece coord game    -- фишка еще не взята
  | otherwise = return $ makeMove coord game    -- фишка уже взята
          where
            coord = getCell mouse
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | gamePlayer game == Black = return $ game     -- если сейчас ходит бот, мы ничего не можем сделать
  | gameEnding game /= Nothing = return $ game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = return $ game    -- фишка еще не взята, отменять нечего
  | otherwise = return $ putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = return $ game

-- | Обновление игры. Вызывает функцию бота
updateGame :: Float -> Game -> IO Game
updateGame _ game
  | gamePlayer game == Beige = return $ game
  | gameEnding game /= Nothing = return $ game
  | gameMovable game /= Nothing = return $ game
  | otherwise = return $ makeMove put $ takePiece take game
    where
      (take, put) = bot game
      bot = futurePlusPrinciplesBot 2 [getCloser 1, getCloser 2, bringIn Ant, bringIn Beetle, bringIn Hopper]
