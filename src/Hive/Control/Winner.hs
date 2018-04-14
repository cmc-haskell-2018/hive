module Hive.Control.Winner
  where

import Hive.Model
import Hive.Config
import qualified Data.Map as Map
import Data.Maybe


-- =========================================
-- Все, что относится к определению победителя
-- =========================================


-- | Установить gameEnding в Game, если игра завершилась
checkWinner :: Game -> Game
checkWinner game = game{gameEnding = winner (gameBoard game)}

-- | Определение победителя
winner :: Board -> Maybe Ending
winner board
  | blackLose && beigeLose = Just Tie
  | blackLose = Just (Win Beige)
  | beigeLose = Just (Win Black)
  | otherwise = Nothing
  where
    blackLose = fromMaybe False $ beeIsLocked board <$> (beeCoord Black board)     -- черная пчела окружена
    beigeLose = fromMaybe False $ beeIsLocked board <$> (beeCoord Beige board)    -- бежевая пчела окружена

-- | Координаты пчелы данного цвета
beeCoord :: Player -> Board -> Maybe Coord
beeCoord player board = takeCoord $ Map.filter hasBee board
  where
    hasBee :: Cell -> Bool      -- есть ли в клетке пчела данного цвета?
    hasBee pieces = filter (\(p, insect, _) -> p == player && insect == Queen) pieces /= []

-- | Берет координаты первого элемента в контейнере (вспомогательная функция)
takeCoord :: Board -> Maybe Coord
takeCoord filtered
  | Map.null filtered = Nothing
  | otherwise = if isSide coord then Nothing
                                        else (Just coord)
    where
      isSide (x, _) = x > n+1 || x < -(n+1)
      coord = fst $ Map.elemAt 0 filtered
      n = numberOfPieces

-- | Проверяет, заперта ли пчела
beeIsLocked :: Board -> Coord -> Bool
beeIsLocked board (x, y) = isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2)
    where
      isNotEmpty (i, j) = Map.member (i, j) board

