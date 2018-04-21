module Hive.Render.Almost
  where

import Hive.Model
import Hive.Control
import qualified Data.Map as Map
import Data.Maybe

-- =========================================
-- Проверить, что команда почти проиграла
-- =========================================

-- | Шах?
aboutToLose :: Game -> Bool
aboutToLose game@Game{gameMovable = Nothing, gameEnding = Nothing, gameBoard = board, gamePlayer = player}
  | move == (0, 1) = False
  | otherwise = or $ fmap (checkHostileGame move) options  -- checkHostileGame
  where
      filtered = Map.toList $ Map.filterWithKey isHostile board
      options = fmap (createHostileGame game) filtered
      coordOfBee = fromMaybe (0, 1)(beeCoord player board)
      i = fst coordOfBee
      j = snd coordOfBee
      move = fromMaybe (0, 1) $ beeIsAlmostLocked coordOfBee board
      
      isHostile :: Coord -> [Piece] -> Bool
      isHostile _ [] = False
      isHostile (x, y)((p,_,_):_) = p /= player && (x == i+1 && y == j+1 || x == i+1 && y == j-1 ||
                                                    x == i-1 && y == j+1 || x == i-1 && y == j-1 ||
                                                    x == i && y == j+2 || x == i && y == j-2) == False
      
aboutToLose _ = False

-- | Проверить, что пчела почти заперта. Если да, то вернуть свободную рядом с ней клетку
beeIsAlmostLocked :: Coord -> Board -> Maybe Coord
beeIsAlmostLocked (0, 1) _ = Nothing        -- если пчела еще не введена в игру
beeIsAlmostLocked (x, y) board
  | up && upLeft && upRight && down && downLeft && downRight = Nothing       -- пчела заперта, этот случай нас не интересует
  | up && upLeft && upRight && down && downLeft = Just (x+1, y-1)
  | up && upLeft && upRight && down && downRight = Just (x-1, y-1)
  | up && upLeft && upRight && downLeft && downRight = Just (x, y-2)
  | up && upLeft && down && downLeft && downRight = Just (x+1, y+1)
  | up && upRight && down && downLeft && downRight = Just (x-1, y+1)
  | upLeft && upRight && down && downLeft && downRight = Just (x, y+2)
  | otherwise = Nothing
    where
      isNotEmpty (i, j) = Map.member (i, j) board
      upLeft = isNotEmpty (x-1, y+1)
      upRight = isNotEmpty (x+1, y+1)
      downLeft = isNotEmpty (x-1, y-1)
      downRight = isNotEmpty (x+1, y-1)
      up = isNotEmpty (x, y+2)
      down = isNotEmpty (x, y-2)


-- | Составить игру с поднятой враждебной фишкой
createHostileGame :: Game -> (Coord, Cell) -> Game
createHostileGame game@Game{gameBoard = board, gamePlayer = player} (coord, pieces)
    = game{gamePlayer = switchPlayer player, gameMovable = Just movable, gameBoard = deleteInsect coord board}
    where
      movable = (coord, head pieces)


-- | Может ли враг выиграть с данной поднятой фишкой?
checkHostileGame :: Coord -> Game -> Bool
checkHostileGame coord game = elem coord (possibleMoves game)
