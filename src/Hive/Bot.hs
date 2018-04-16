module Hive.Bot
  where

import Hive.Model
import Hive.Control
import Data.Maybe
import qualified Data.Map as Map
import Data.Char

-- | Хороший ли это выбор хода?
data Choice = Good | Neutral | Bad
  deriving (Eq)

-- | Ход - откуда, куда, хороший ли выбор
type Move = (Choice, (Coord, Coord))


-- =========================================
-- Функции ботов
-- =========================================


-- | Бот, просчитывающий ходы
futureMovesBot :: Game -> (Coord, Coord)
futureMovesBot game
  | good /= Nothing = get good
  | neutral /= [] = takeAt neutral $ mod (genRand game) (length neutral)
  | otherwise = get bad
  where
    player = gamePlayer game
    board = gameBoard game
    filtered = map fst $ Map.toList $ Map.filter (\((p,_,_):_) -> p == player) board
    moves = foldr (++) [] $ fmap (createMovesForPiece game) filtered
    good = lookup Good moves -- хороший ход
    neutral = map snd $ filter (\(choice,_) -> choice == Neutral) moves -- список нейтральных ходов
    bad = lookup Bad moves -- плохой ход
    get = fromMaybe ((0,1),(0,1))

-- | Бот, просчитывающий ходы и руководствующийся принципами
futurePlusPrinciplesBot :: Game -> (Coord, Coord)
futurePlusPrinciplesBot game
  | good /= Nothing = get good
  | neutral /= [] && coordOfBee == (0,1) = takeAt neutral $ mod (genRand game) (length neutral)
  | neutral /= [] && closer /= [] = head closer
  | neutral /= [] = takeAt neutral $ mod (genRand game) (length neutral)
  | otherwise = get bad
  where
    player = gamePlayer game
    opponent = if player == Beige then Black else Beige
    board = gameBoard game
    filtered = map fst $ Map.toList $ Map.filter (\((p,_,_):_) -> p == player) board
    moves = foldr (++) [] $ fmap (createMovesForPiece game) filtered
    good = lookup Good moves -- хороший ход
    neutral = map snd $ filter (\(choice,_) -> choice == Neutral) moves -- список нейтральных ходов
    bad = lookup Bad moves -- плохой ход
    get = fromMaybe ((0,1),(0,1))
    coordOfBee = fromMaybe (0,1) $ takeCoord $ Map.filter (\((p, ins, _):_) -> p == opponent && ins == Queen) board     -- координата вражеской пчелы
    close n = filter (\(from, to) -> distance coordOfBee to == n
        && distance coordOfBee from > distance coordOfBee to) neutral      -- насекомое подходит на расстояние n от пчелы
    closer = (close 0) ++ (close 1) ++ (close 2) ++ (close 3)

-- | Расстояние между клетками
distance :: (Coord) -> (Coord) -> Int
distance (x1, y1) (x2, y2) = (abs (x2 - x1) + abs (y2 - y1)) `div` 2
    
-- | Взятие элемента из списка по номеру
takeAt :: [(Coord, Coord)] -> Int -> (Coord, Coord)
takeAt [] _ = ((0,1),(0,1))
takeAt (x:_) 0 = x
takeAt (_:xs) n = takeAt xs (n-1)

-- | Составить список Move для данной поднятой фишки
createMovesForPiece :: Game -> Coord -> [Move]
createMovesForPiece game coord
  = fmap makeTuple possible
    where
      newGame = takePiece coord game
      possible = possibleMoves newGame
      makeTuple = (\move -> (tryPutting 1 newGame move, (coord, move)))

-- | Посмотреть, что будет, если взять фишку
tryTaking :: Int -> Game -> Coord -> Choice
tryTaking n game coord = combineChoices $ fmap (tryPutting n newGame) possible
  where
    newGame = takePiece coord game
    possible = possibleMoves newGame

-- | Посмотреть, что будет, если положить фишку
tryPutting :: Int -> Game -> Coord -> Choice
tryPutting n game coord
  | newEnding == Just (Win player) = Good
  | newEnding == Just (Win opponent) = Bad
  | newEnding == Just Tie = Neutral
  | n == 0 = Neutral
  | player == newPlayer = combineChoices choices
  | otherwise = reverseChoices choices
    where
      player = gamePlayer game
      opponent = if player == Beige then Black else Beige
      newGame = makeMove coord game
      newEnding = gameEnding newGame
      newBoard = gameBoard game
      newPlayer = gamePlayer newGame
      filtered = map fst $ Map.toList $ Map.filter (\((p,_,_):_) -> p == newPlayer) newBoard        -- координаты фишек нового игрока
      choices = fmap (tryTaking (n-1) newGame) filtered     -- характеристика ходов нового игрока

-- | Обобщение оптимальности выбора по возможным вариантам хода
combineChoices :: [Choice] -> Choice
combineChoices choices
  | elem Good choices = Good
  | elem Neutral choices = Neutral
  | otherwise = Bad

-- | Оптимальность выбора по возможным вариантам хода противника
reverseChoices :: [Choice] -> Choice
reverseChoices choices
  | elem Good choices = Bad
  | elem Neutral choices = Neutral
  | otherwise = Good

-- | Сгенерировать (типа) случайное число, зависящее от игры
genRand :: Game -> Int
genRand game = mod (sum $ fmap ord $ show game) 1023

