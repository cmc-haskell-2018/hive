module Hive.Bot
  where

import Hive.Model
import Hive.Config
import Hive.Control
import Data.Maybe
import qualified Data.Map as Map
import Data.Char

-- | Хороший ли это выбор хода?
data Choice = Good | Neutral | Bad
  deriving (Eq)

-- | Откуда - куда
type FromTo = (Coord, Coord)
 
-- | Ход -  хороший ли выбор, откуда, куда
type Move = (Choice, FromTo)

type Bot = Game -> FromTo

type Principle = Game -> [FromTo] -> Maybe FromTo


-- =========================================
-- Основная Функция бота
-- =========================================


-- | Бот, просчитывающий ходы и руководствующийся принципами
futurePlusPrinciplesBot :: Int -> [Principle] -> Game -> FromTo
futurePlusPrinciplesBot steps principles game@Game{gamePlayer = player, gameBoard = board}
  | good /= Nothing = get good
  | neutral /= [] = makeNeutralMove principles neutral game
  | notStupid /= [] = head notStupid
  | otherwise = get $ lookup Bad moves
  where
    filtered = map fst $ Map.toList $ Map.filter (\((p,_,_):_) -> p == player) board        -- коогдинаты открытых фишек игрока
    moves = foldr (++) [] $ fmap (createMovesForPiece steps game) filtered      -- все возможные ходы игрока
    good = lookup Good moves -- хороший ход
    neutral = map snd $ filter (\(choice,_) -> choice == Neutral) moves -- список нейтральных ходов
    myBee = fromMaybe (0,1) $ beeCoord player board     -- координата моей пчелы
    notStupid = map snd $ filter (\(_,(_, to)) -> distance myBee to > 1) moves -- список не тупых ходов (не приводящих к проигрышу сразу)
    get = fromMaybe ((0,1),(0,1))
    

-- =========================================
-- Принципы бота
-- =========================================


-- | Выбрать нейтральный ход
makeNeutralMove :: [Principle] -> [FromTo] -> Game -> FromTo
makeNeutralMove [] neutral game = randomMove game neutral
makeNeutralMove (f:fs) neutral game
  | move == Nothing = makeNeutralMove fs neutral game
  | otherwise = get move
  where
    get = fromMaybe ((0,1),(0,1))
    move = f game neutral

-- | Взять ход из списка рандомно
randomMove :: Game -> [FromTo] -> FromTo
randomMove game neutral = takeAt neutral $ mod genRand (length neutral)
  where
    -- | Взятие элемента из списка по номеру
    takeAt :: [FromTo] -> Int -> FromTo
    takeAt [] _ = ((0,1),(0,1))
    takeAt (x:_) 0 = x
    takeAt (_:xs) n = takeAt xs (n-1)
    
    -- | Сгенерировать число, зависящее от игры
    genRand = mod (sum $ fmap ord $ show game) 1023

-- | Закрыть вражескую пчелу жуком
coverBee :: Principle
coverBee Game{gamePlayer = player, gameBoard = board} neutral
  | openBee /= Nothing && beetle /= [] = Just (head beetle)
  | otherwise = Nothing
  where
    opponent = if player == Beige then Black else Beige
    openBee = takeCoord $ Map.filter (\((p, ins, _):_) -> p == opponent && ins == Queen) board     -- открытая координата вражеской пчелы
    beetle = filter (\(_, to) -> distance (fromMaybe (0,1) openBee) to == 0) neutral

-- | Подойти на расстояние n к вражеской пчеле
getCloser :: Int -> Principle
getCloser n Game{gamePlayer = player, gameBoard = board} neutral
  | bee /= (0,1) && close /= [] = Just (head close)
  | otherwise = Nothing
  where
    opponent = if player == Beige then Black else Beige
    bee = fromMaybe (0,1) $ beeCoord opponent board     -- координата вражеской пчелы
    myOpenBee = fromMaybe (0,1) $ takeCoord $ Map.filter (\((p, ins, _):_) -> p == player && ins == Queen) board     -- моя открытая пчела
    close = filter (\(from, to) -> distance bee to == n && to /= myOpenBee
        && distance bee from > distance bee to) neutral      -- насекомое подходит на расстояние n от пчелы

-- | Ввести в игру нужное насекомое
bringIn :: Insect -> Principle
bringIn insect Game{gameBoard = board} neutral
  | insects /= [] = Just (head insects)
  | otherwise = Nothing
  where
    insects = filter (\(from,_) -> isSide from && isInsect (Map.lookup from board)) neutral
    isSide (x, _) = x < -(n + 1) || x > n + 1
    isInsect :: Maybe Cell -> Bool
    isInsect (Just ((_,ins,_):_)) = ins == insect
    isInsect _ = False
    n = numberOfPieces

-- | Расстояние между клетками
distance :: (Coord) -> (Coord) -> Int
distance(x1, y1) (x2, y2)
  | isSide x1 || isSide x2 || (x1 + y1) `mod` 2 /= 0 || (x2 + y2) `mod` 2 /= 0 = 1000
  | y >= x = (x + y) `div` 2
  | otherwise = x
    where
      isSide x' = x' < -(n + 1) || x' > n + 1
      x = abs (x2 - x1)
      y = abs (y2 - y1)
      n = numberOfPieces


-- =========================================
-- Просчет ходов
-- =========================================

-- | Составить список Move для данной поднятой фишки
createMovesForPiece :: Int -> Game -> Coord -> [Move]
createMovesForPiece steps game coord
  = fmap makeTuple possible
    where
      newGame = takePiece coord game
      possible = possibleMoves newGame
      makeTuple = (\move -> (tryPutting steps newGame move, (coord, move)))

-- | Посмотреть, что будет, если взять фишку
tryTaking :: Int -> Game -> Coord -> Choice
tryTaking n game coord = combineChoices $ fmap (tryPutting n newGame) possible
  where
    newGame = takePiece coord game
    possible = possibleMoves newGame

-- | Посмотреть, что будет, если положить фишку
tryPutting :: Int -> Game -> Coord -> Choice
tryPutting n game coord
  | n <= 0 = Neutral
  | newEnding == Just (Win player) = Good
  | newEnding == Just (Win opponent) = Bad
  | newEnding == Just Tie = Neutral
  | n == 1 = Neutral
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
