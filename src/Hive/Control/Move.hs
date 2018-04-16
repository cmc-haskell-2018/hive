module Hive.Control.Move
  where

import Hive.Model
import Hive.Config
import Hive.Control.Moves
import qualified Data.Map as Map
import Data.Maybe


-- =========================================
-- Перемещение фишек
-- =========================================


-- | Положить фишку на место
putPieceBack :: Game -> Game
putPieceBack game@Game{gameMovable = Just (coord, piece), gameBoard = board}
  = game{gameMovable = Nothing, gameBoard = putInsect piece coord board}  
putPieceBack game = game    -- чтобы компилятор не ругался


-- | Взять фишку с координатами под мышкой, если возможно
takePiece :: Coord -> Game -> Game
takePiece coord game@Game{gamePlayer = player, gameBoard = board, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  | pieces == [] = game     -- фишки с такими координатами нет
  | pieceColor top /= player = game
  | checkQueenStep movable = newGame
  | step == Fours = game
  | otherwise = newGame
  where
    newGame = Game
      { gameBoard = deleteInsect coord board
      , gamePlayer = player
      , gameMovable = Just movable
      , gameEnding = Nothing
      , gameStepBlack = stepBlack
      , gameStepBeige = stepBeige
      }
    step = if player == Black then stepBlack else stepBeige
    pieces = fromMaybe [] $ Map.lookup coord board    -- список фишек в клетке с нужными координатами
    top = head pieces    -- самая верхняя фишка в списке
    movable = (coord, top)
    pieceColor (p, _, _) = p
    checkQueenStep :: Movable -> Bool
    checkQueenStep ( (_,_), (_,ins,_)) = ins == Queen       -- взяли пчелу

-- | Удаление фишки из старой позиции (перед перемещением)
deleteInsect :: Coord -> Board -> Board
deleteInsect coord board
  | tail pieces == [] = Map.delete coord board
  | otherwise = Map.adjust tail coord board
  where
    pieces = fromMaybe [] $ Map.lookup coord board    -- список фишек в клетке с нужными координатами

-- | Сделать ход, если возможно
makeMove :: Coord -> Game -> Game
makeMove coord game@Game{gamePlayer = player, gameBoard = board, gameMovable = Just movable, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
   | (elem coord (possibleMoves game)) = checkWinner $ shiftGame Game    -- если выбранный ход возможен
     { gamePlayer = switchPlayer player
     , gameBoard = putInsect (snd movable) coord board
     , gameMovable = Nothing
     , gameEnding = Nothing
     , gameStepBlack = if player == Black then nextStep stepBlack else stepBlack
     , gameStepBeige = if player == Beige then nextStep stepBeige else stepBeige
     }
   | otherwise = game    -- если выбранный ход невозможен
     where 
       nextStep :: Step -> Step
       nextStep x 
         |(\(_,(_,ins,_)) -> ins) movable == Queen = Other
         | x == Other = Other
         | otherwise = succ x
makeMove _ game = game    -- чтобы компилятор не ругался

-- | Поставить фишку
putInsect :: Piece -> Coord -> Board -> Board
putInsect piece coord board
  | Map.member coord board = Map.adjust (piece:) coord board
  | otherwise = Map.insert coord [piece] board


-- =========================================
-- По мелочи
-- =========================================


-- | Сменить текущего игрока
switchPlayer :: Player -> Player
switchPlayer Beige = Black
switchPlayer Black = Beige


-- =========================================
-- Все, что относится к сдвигу массива фишек
-- =========================================


-- | Это просто для вызова shiftBoard
shiftGame  :: Game -> Game
shiftGame game@Game{gameBoard = board} = game{gameBoard = shiftBoard board}

-- | Передвинуть массив фишек, если он касается края поля
shiftBoard :: Board -> Board
shiftBoard board
  | Map.filterWithKey (\(x, _) _ -> x == -(n+1)) board /= Map.empty    --  если коснулись левой границы
      = shiftBoard $ shiftCoord (1, 1) board
  | Map.filterWithKey (\(x, y) _ -> x >= -(n+1) && y-x == 2*(n+1)) board /= Map.empty    --  если коснулись левой верхней границы
      = shiftBoard $ shiftCoord (1, -1) board
  | Map.filterWithKey (\(x, y) _ -> x <= n+1 && x+y == 2*(n+1)) board /= Map.empty    --  если коснулись правой верхней границы
      = shiftBoard $ shiftCoord (0, -2) board
  | Map.filterWithKey (\(x, _) _ -> x == n+1) board /= Map.empty    --  если коснулись правой границы
      = shiftBoard $shiftCoord (-1, -1) board
  | Map.filterWithKey (\(x, y) _ -> x <= n+1 && x-y == 2*(n+1)) board /= Map.empty    --  если коснулись правой нижней границы
      = shiftBoard $ shiftCoord (-1, 1) board
  | Map.filterWithKey (\(x, y) _ -> x >= -(n+1) && x+y == -2*(n+1)) board /= Map.empty    --  если коснулись левой нижней границы
      = shiftBoard $ shiftCoord (0, 2) board
  | otherwise = board
    where
      shiftCoord :: (Int, Int) -> Board -> Board    -- сдвигает поле на (i, j)
      shiftCoord (i, j) = Map.mapKeys (\(x, y) -> if (x>= -(n+1)) && (x<= n+1)
                                                    then(x+i, y+j) else (x, y))
      n = numberOfPieces


-- =========================================
-- Все, что относится к определению победителя
-- =========================================


-- | Установить gameEnding в Game, если игра завершилась; сменить текущего игрока, если нечем ходить
checkWinner :: Game -> Game
checkWinner game -- = game{gameEnding = winner (gameBoard game) (gamePlayer game)}
  | blackLocked && beigeLocked = game{gameEnding = Just Tie}
  | blackLocked = game{gameEnding = Just (Win Beige)}
  | beigeLocked = game{gameEnding = Just (Win Black)}
  | possibleCurrent /= [] = game
  | possibleOpponent /= [] = changePlayer
  | otherwise = game{gameEnding = Just Tie}
  where
    board = gameBoard game
    current = gamePlayer game
    opponent = switchPlayer current
    blackLocked = beeIsLocked board (beeCoord Black board)     -- черная пчела окружена
    beigeLocked = beeIsLocked board (beeCoord Beige board)    -- бежевая пчела окружена
    filtered player = map fst $ Map.toList $ Map.filter (\((p,_,_):_) -> p == player) board        -- координаты фишек текущего игрока
    possibleCurrent = foldr (++) [] $ fmap (possibleMoves.(flip takePiece game)) (filtered current)
    possibleOpponent = foldr (++) [] $ fmap (possibleMoves.(flip takePiece changePlayer)) (filtered opponent)
    changePlayer = game{gamePlayer = opponent}


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
beeIsLocked :: Board -> Maybe Coord -> Bool
beeIsLocked _ Nothing = False
beeIsLocked board (Just (x, y)) = isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2)
    where
      isNotEmpty (i, j) = Map.member (i, j) board

