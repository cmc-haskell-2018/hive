module Hive.Control.Move
  where

import Hive.Model
import Hive.Config
import Hive.Control.Winner
import Hive.Control.Moves
import qualified Data.Map as Map
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe


-- =========================================
-- Перемещение фишек
-- =========================================


-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ shiftGame $ 
        makeMove (getCell mouse) game    -- фишка уже взята
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = game    -- фишка еще не взята, отменять нечего
  | otherwise = putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = game

-- | Положить фишку на место
putPieceBack :: Game -> Game
putPieceBack game@Game{gameMovable = Just (coord, piece), gameBoard = board}
  = game{gameMovable = Nothing, gameBoard = putInsect piece coord board}  
putPieceBack game = game    -- чтобы компилятор не ругался

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

-- | Взять фишку с координатами под мышкой, если возможно
takePiece :: Point -> Game -> Game
takePiece (x, y) game@Game{gamePlayer = player, gameBoard = board, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
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
    coord = getCell (x, y)
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
   | (elem coord (possibleMoves game)) = Game    -- если выбранный ход возможен
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

-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id


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