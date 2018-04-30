module Main where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically, newTVarIO, readTVarIO)
import Control.Concurrent.STM.TVar.Lifted (writeTVarIO)

import Hive
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  game <- initGame
  gameVar <- newTVarIO game
  forkBot Beige bot1 gameVar
  forkBot Black bot2 gameVar
  playIO display bgColor fps gameVar (fmap drawGame . readTVarIO) (const pure) (const pure)
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 1      -- кол-во кадров в секунду
    
    bot1 = futurePlusPrinciplesBot 2 [getCloser 1, getCloser 2, bringIn Ant, bringIn Beetle, bringIn Hopper]
    bot2 = futurePlusPrinciplesBot 0 []
    
-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> IO Game
handleGame _ game = pure game

forkBot :: Player -> Bot -> TVar Game -> IO ()
forkBot p bot gameVar = do
    forkIO (forever step)
    return ()
    where
      step = do
        threadDelay (10)
        game <- readTVarIO gameVar
        if (gamePlayer game == p && gameEnding game == Nothing)
            then let (botTake, botPut) = bot game
                     newGame = makeMove botPut (takePiece botTake game)
                 in threadDelay (10^6) >>
                   writeTVarIO gameVar newGame
            else return ()

-- | Обновление игры. Вызывает функцию бота
updateGame :: Float -> Game -> IO Game
updateGame _ = pure