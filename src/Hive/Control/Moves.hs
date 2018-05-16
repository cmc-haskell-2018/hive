module Hive.Control.Moves
  where

import Hive.Model
import Hive.Config
import qualified Data.Map as Map
import Data.List
import Data.Maybe


-- =========================================
-- Общие функции для определения возможных ходов
-- =========================================


-- | Список координат всех допустимых клеток для постановки фишки
possibleMoves :: Game -> [Coord]
possibleMoves Game{gameBoard = board, gameMovable = Just((x,y), (player, ins,_)), gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  | isSide == False && doesNotTear (x, y) board == False = []        -- если взятая фишка разрывает улей
  | player == Beige && step == First = [(0,0)]      -- первый ход бежевых
  | player == Black && step == First = [(0,2), (1,1), (1,-1), (0,-2), (-1,-1), (-1,1)]     -- первый ход черных
  | isSide = notTouchingPieces (switchPlayer player) board $ haveNeighbours board
  | step /= Other = []
  | ins == Queen  = queen_cells (x,y) board
  | ins == Beetle = beetle_cells (x,y) board
  | ins == Hopper = hopper_cells (x,y) board
  | ins == Ant = antMoves (x, y) board
  | ins == LadyBug = ladyBugMoves (x, y) board
  | ins == Mosquito = mosquitoMoves (x, y) board
  | otherwise = spiderMoves (x, y) board
 where
  step = if player == Beige then stepBeige else stepBlack
  isSide = x > n+1 || x < -(n+1)
  n = numberOfPieces
  switchPlayer Beige = Black
  switchPlayer Black = Beige
possibleMoves _ = []

-- | Свободные клетки, у которых в соседних есть фишки
haveNeighbours :: Board -> [Coord]
haveNeighbours board = nub (foldr f [] inside) \\ inside
  where
    inside = map fst $ Map.toList $ Map.filterWithKey (\(x, _) _ -> x <= n+1 && x >= -(n+1)) board       -- координаты фишек на поле
    f = (\(x, y) -> (++)[(x+1, y+1),(x+1, y-1),(x-1, y+1),(x-1, y-1),(x, y+2),(x, y-2)])
    n = numberOfPieces
    
-- | удаляет из списка координат стартовые клетки
delStartCells :: [Coord] -> [Coord]
delStartCells [] = []
delStartCells l = filter (\(x,_) -> x >= -(n+1) && x <= n+1 ) l
 where n = numberOfPieces

-- | Фишки, только что вводимые в игру, не должны касаться фишек другого игрока
notTouchingPieces :: Player -> Board -> [Coord] -> [Coord]
notTouchingPieces player board = filter (\coord -> doesNotTouch coord player board)

-- | Проверить, что фишка не касается враждебной
doesNotTouch :: Coord -> Player -> Board -> Bool
doesNotTouch (i, j) player board = Map.filterWithKey (\(x, y) pieces -> (x==i&&y==j+2||x==i&&y==j-2||x==i+1&&y==j+1||x==i+1&&y==j-1||
                                                                         x==i-1&&y==j+1||x==i-1&&y==j-1)&& isHostile pieces) board == Map.empty
  where
    isHostile :: [Piece] -> Bool
    isHostile [] = False
    isHostile ((p,_,_):_) = p == player


-- =========================================
-- Все, что относится к проверке на неразрывность
-- =========================================


-- | Проверить, что при поднятии фишки улей не разрывается
doesNotTear :: Coord -> Board -> Bool
doesNotTear (i, j) board
  | neighbours == [] = True
  | otherwise = and $ map (flip Map.member accessible) (tail neighbours)     -- проверить, что все соседи достижимы из первого соседа
  where
    neighbours :: [Coord]
    neighbours = map fst $ Map.toList $ Map.filterWithKey (\(x, y) _ -> x == i+1 && y == j+1 || x == i+1 && y == j-1 ||     -- соседние заполненные клетки
                                                                           x == i-1 && y == j+1 || x == i-1 && y == j-1 ||
                                                                           x == i && y == j+2 || x == i && y == j-2) board
    accessible = accessibleCells (head neighbours) board        -- клетки, достижимые из первого соседа

-- | Клетки с фишками, достижимые из данной клетки
accessibleCells :: Coord -> Board -> AccCells
accessibleCells (x, y) board =
  checkForPieces (x, y+2) board $
  checkForPieces (x+1, y+1) board $
  checkForPieces (x-1, y+1) board $
  checkForPieces (x, y-2) board $
  checkForPieces (x-1, y-1) board $
  checkForPieces (x+1, y-1) board Map.empty

-- | Проверяем фишки вокруг заданной клетки
checkForPieces :: Coord -> Board -> AccCells -> AccCells
checkForPieces coord@(x, y) board accCells
  | Map.notMember coord board = accCells
  | Map.member coord accCells = accCells
  | otherwise = 
    checkForPieces (x-1, y+1) board $
    checkForPieces (x, y+2) board $
    checkForPieces (x+1, y+1) board $
    checkForPieces (x+1, y-1) board $
    checkForPieces (x, y-2) board $
    checkForPieces (x-1, y-1) board newAccCells
      where
        newAccCells = if pieces == [] then accCells else Map.insert coord pieces accCells     -- вставить текущую клетку в составляемый список
        pieces = fromMaybe [] (Map.lookup coord board)


-- =========================================
-- Пчела и жук
-- =========================================


-- | координаты для королевы и жука
-- |Пчеломатка может перемещаться всего на 1 "клетку". Жук, также как и пчеломатка, может перемещаться только на 1 позицию за
-- |ход. Но в отличии от всех остальных фишек, он может перемещать поверх других фишек.
queen_cells :: Coord -> Board -> [Coord]
queen_cells (x,y) board = coord1 ++ coord2 ++ coord3 ++ coord4 ++ coord5 ++ coord6
 where 
    coord1 = if Map.notMember (x, y+2)   board && (Map.notMember (x+1, y+1) board && Map.member (x-1, y+1) board
                                                              || Map.member (x+1, y+1) board && Map.notMember (x-1, y+1) board)
                                                              then [(x,y+2)] else [] 
    coord2 = if Map.notMember (x+1, y+1) board &&  (Map.notMember (x, y+2)  board && Map.member (x+1, y-1) board
                                                              || Map.member (x, y+2)  board && Map.notMember (x+1, y-1) board)                             
                                                              then [(x+1,y+1)] else []
    coord3 = if Map.notMember (x+1, y-1) board &&  (Map.notMember (x+1, y+1)board && Map.member (x, y-2) board
                                                              || Map.member (x+1, y+1) board && Map.notMember (x, y-2)  board) 
                                                              then [(x+1,y-1)] else []
    coord4 = if Map.notMember (x, y-2)   board &&  (Map.notMember (x+1, y-1) board && Map.member (x-1, y-1) board
                                                              || Map.member (x+1, y-1) board && Map.notMember (x-1, y-1) board)
                                                              then [(x,y-2)] else []
    coord5 = if Map.notMember (x-1, y-1) board &&  (Map.notMember (x, y-2)  board && Map.member (x-1, y+1) board
                                                              || Map.member (x, y-2)  board && Map.notMember (x-1, y+1) board)
                                                              then [(x-1,y-1)] else [] 
    coord6 = if Map.notMember (x-1, y+1) board &&  (Map.notMember (x-1, y-1)board && Map.member (x, y+2) board
                                                              || Map.member (x-1, y-1)board && Map.notMember (x, y+2)  board)
                                                              then [(x-1,y+1)] else []

beetle_cells :: Coord -> Board -> [Coord]
beetle_cells (x,y) board = coord1 ++ coord2 ++ coord3 ++ coord4 ++ coord5 ++ coord6
 where 
    coord1 = if Map.member (x, y+2)   board || Map.member (x-1, y+1) board || Map.member (x+1, y+1) board
                                                              then [(x,y+2)] else [] 
    coord2 = if Map.member (x+1, y+1) board || Map.member (x+1, y-1) board || Map.member (x, y+2)  board                             
                                                              then [(x+1,y+1)] else []
    coord3 = if Map.member (x+1, y-1) board || Map.member (x, y-2) board || Map.member (x+1, y+1) board 
                                                              then [(x+1,y-1)] else []
    coord4 = if Map.member (x, y-2)   board || Map.member (x-1, y-1) board || Map.member (x+1, y-1) board
                                                              then [(x,y-2)] else []
    coord5 = if Map.member (x-1, y-1) board || Map.member (x-1, y+1) board || Map.member (x, y-2) board
                                                              then [(x-1,y-1)] else [] 
    coord6 = if Map.member (x-1, y+1) board || Map.member (x, y+2) board || Map.member (x-1, y-1) board
                                                              then [(x-1,y+1)] else [] 
-- ======================================
-- Божья коровка
-- ======================================
ladyBugMoves :: Coord -> Board -> [Coord] -- Запуск подбора ходов
ladyBugMoves coord board = ladyStep 0 coord board [coord]

ladyStep :: Int -> Coord -> Board -> [Coord] -> [Coord] -- Рекурсивный выбор клеток
ladyStep 3 coord _ place = if not (elem coord place) then [coord] else []
ladyStep 2 (x, y) board place = nub $ collectUpE ++ collectUpLeftE ++ collectUpRightE ++
                          collectDownE ++ collectDownLeftE ++ collectDownRightE
  where
    isEmpty c = Map.notMember c board    -- пустая ли клетка
    up = (x, y+2)    -- верхняя клетка
    upLeft = (x-1, y+1)    -- верхняя левая клетка
    upRight = (x+1, y+1)   -- верхняя правая клетка
    down = (x, y-2)    -- нижняя клетка
    downLeft = (x-1, y-1)    -- нижняя левая клетка
    downRight = (x+1, y-1)    -- нижняя правая клетка

    collectUpE = if isEmpty up then ladyStep 3 up board place else []
    collectUpLeftE = if isEmpty upLeft then ladyStep 3 upLeft board place else []
    collectUpRightE = if isEmpty upRight then ladyStep 3 upRight board place else []
    collectDownE = if isEmpty down then ladyStep 3 down board place else []
    collectDownLeftE = if isEmpty downLeft then ladyStep 3 downLeft board place else []
    collectDownRightE = if isEmpty downRight then ladyStep 3 downRight board place else []

ladyStep n (x, y) board place = nub $ collectUpF ++ collectUpLeftF ++ collectUpRightF ++
                          collectDownF ++ collectDownLeftF ++ collectDownRightF 
  where
    isFull c = Map.member c board    -- заполнена ли клетка
    up = (x, y+2)    -- верхняя клетка
    upLeft = (x-1, y+1)    -- верхняя левая клетка
    upRight = (x+1, y+1)   -- верхняя правая клетка
    down = (x, y-2)    -- нижняя клетка
    downLeft = (x-1, y-1)    -- нижняя левая клетка
    downRight = (x+1, y-1)    -- нижняя правая клетка 

    collectUpF = if isFull up then ladyStep (n+1) up board place else [] 
    collectUpLeftF = if isFull upLeft then ladyStep (n+1) upLeft board place else []
    collectUpRightF = if isFull upRight then ladyStep (n+1) upRight board place else []
    collectDownF = if isFull down then ladyStep (n+1) down board place else []
    collectDownLeftF = if isFull downLeft then ladyStep (n+1) downLeft board place else []
    collectDownRightF = if isFull downRight then ladyStep (n+1) downRight board place else []

    
-- =====================================
-- Комар, или москит
-- ======================================
mosquitoMoves :: Coord -> Board -> [Coord]
mosquitoMoves (x, y) board = nub $ neighborUp ++ neighborUpLeft ++
                             neighborUpRight ++ neighborDown ++
                             neighborDownLeft ++ neighborDownRight
  where
    isFull c = Map.member c board    -- заполнена ли клетка
    up = (x, y+2)    -- верхняя клетка
    upLeft = (x-1, y+1)    -- верхняя левая клетка
    upRight = (x+1, y+1)   -- верхняя правая клетка
    down = (x, y-2)    -- нижняя клетка
    downLeft = (x-1, y-1)    -- нижняя левая клетка
    downRight = (x+1, y-1)    -- нижняя правая клетка
    -- pieces coord = fromMaybe [] $ Map.lookup coord board 
    -- top coord = head pieces coord
    takeIns :: Piece -> Insect
    takeIns (_,ins,_) = ins
    neighborUp = if isFull up 
      then neighbor (takeIns (head (fromMaybe [] $ Map.lookup up board))) (x, y) board 
      else []
    neighborUpLeft = if isFull upLeft 
      then neighbor (takeIns (head (fromMaybe [] $ Map.lookup upLeft board))) (x, y) board 
      else []
    neighborUpRight = if isFull upRight 
      then  neighbor (takeIns (head (fromMaybe [] $ Map.lookup upRight board))) (x, y) board 
      else []
    neighborDown = if isFull down 
      then neighbor (takeIns (head (fromMaybe [] $ Map.lookup down board))) (x, y) board 
      else []
    neighborDownLeft = if isFull downLeft 
      then neighbor (takeIns (head (fromMaybe [] $ Map.lookup downLeft board))) (x, y) board
      else []
    neighborDownRight = if isFull downRight 
      then neighbor (takeIns (head (fromMaybe [] $ Map.lookup downRight board))) (x, y) board 
      else []
    
neighbor :: Insect -> Coord -> Board -> [Coord]
neighbor ins (x, y) board
  | ins == Queen  = queen_cells (x,y) board
  | ins == Beetle = beetle_cells (x,y) board
  | ins == Hopper = hopper_cells (x,y) board
  | ins == Ant = antMoves (x, y) board
  | ins == LadyBug = ladyBugMoves (x, y) board
  | ins == Spider = spiderMoves (x, y) board
  | otherwise = []

-- Все, что относится к пауку
-- =========================================

-- | Возможные ходы для паука
spiderMoves :: Coord -> Board -> [Coord]
spiderMoves coord board = spiderSteps 0 coord board [coord] 

-- | Рекурсивно собираем возможные ходы паука
spiderSteps :: Int -> Coord -> Board -> [Coord] -> [Coord]
spiderSteps 3 coord _ _ = [coord]
spiderSteps n (x, y) board passed = nub $ collectUp ++ collectUpLeft ++ collectUpRight ++
                                    collectDown ++ collectDownLeft ++ collectDownRight
  where
    isEmpty c = Map.notMember c board    -- пустая ли клетка
    isFull c = Map.member c board    -- заполнена ли клетка
    up = (x, y+2)    -- верхняя клетка
    upLeft = (x-1, y+1)    -- верхняя левая клетка
    upRight = (x+1, y+1)   -- верхняя правая клетка
    down = (x, y-2)    -- нижняя клетка
    downLeft = (x-1, y-1)    -- нижняя левая клетка
    downRight = (x+1, y-1)    -- нижняя правая клетка
    
    collectUp = if isEmpty up && (isEmpty upLeft && isFull upRight || isFull upLeft && isEmpty upRight)
                        && not (elem up passed)       -- собрать ходы сверху
                    then spiderSteps (n+1) up board (up:passed) else []
    collectUpLeft = if isEmpty upLeft && (isEmpty up && isFull downLeft || isEmpty downLeft && isFull up)
                        && not (elem upLeft passed)       -- собрать ходы сверху слева
                    then spiderSteps (n+1) upLeft board (upLeft:passed) else []
    collectUpRight = if isEmpty upRight && (isEmpty up && isFull downRight || isEmpty downRight && isFull up) 
                        && not (elem upRight passed)       -- собрать ходы сверху справа
                    then spiderSteps (n+1) upRight board (upRight:passed) else []
    collectDown = if isEmpty down && (isEmpty downLeft && isFull downRight || isEmpty downRight && isFull downLeft)
                        && not (elem down passed)       -- собрать ходы снизу
                    then spiderSteps (n+1) down board (down:passed) else []
    collectDownLeft = if isEmpty downLeft && (isEmpty upLeft  && isFull down || isEmpty down && isFull upLeft)
                        && not (elem downLeft passed)       -- собрать ходы снизу слева
                    then spiderSteps (n+1) downLeft board (downLeft:passed) else []
    collectDownRight = if isEmpty downRight && (isEmpty down  && isFull upRight || isEmpty upRight && isFull down)
                        && not (elem downRight passed)       -- собрать ходы снизу справа
                    then spiderSteps (n+1) downRight board (downRight:passed) else []


-- =========================================
-- Все, что относится к муравью
-- =========================================


-- | Возможные ходы для муравья
antMoves :: Coord -> Board -> [Coord]
antMoves (x, y) board = delete (x, y) $ collectAntMoves (x, y) board []

-- | Рекурсивно собираем возможные ходы для муравья
collectAntMoves :: Coord -> Board -> [Coord] -> [Coord]
collectAntMoves (x, y) board accumulator = nub $ collectUp $ collectUpLeft $ collectUpRight $
                                           collectDown $ collectDownLeft $ collectDownRight accumulator
  where
    isEmpty c = Map.notMember c board    -- пустая ли клетка
    isFull c = Map.member c board    -- заполнена ли клетка
    up = (x, y+2)    -- верхняя клетка
    upLeft = (x-1, y+1)    -- верхняя левая клетка
    upRight = (x+1, y+1)    -- верхняя правая клетка
    down = (x, y-2)    -- нижняя клетка
    downLeft = (x-1, y-1)    -- нижняя левая клетка
    downRight = (x+1, y-1)    -- нижняя правая клетка
    
    collectUp acc = if isEmpty up && (isEmpty upLeft && isFull upRight || isFull upLeft && isEmpty upRight)
                        && not (elem up acc)       -- собрать ходы сверху
                    then collectAntMoves up board (up:acc) else acc
    collectUpLeft acc = if isEmpty upLeft && (isEmpty up && isFull downLeft || isEmpty downLeft && isFull up)
                        && not (elem upLeft acc)       -- собрать ходы сверху слева
                    then collectAntMoves upLeft board (upLeft:acc) else acc
    collectUpRight acc = if isEmpty upRight && (isEmpty up && isFull downRight || isEmpty downRight && isFull up) 
                        && not (elem upRight acc)       -- собрать ходы сверху справа
                    then collectAntMoves upRight board (upRight:acc) else acc
    collectDown acc = if isEmpty down && (isEmpty downLeft && isFull downRight || isEmpty downRight && isFull downLeft)
                        && not (elem down acc)       -- собрать ходы снизу
                    then collectAntMoves down board (down:acc) else acc
    collectDownLeft acc = if isEmpty downLeft && (isEmpty upLeft  && isFull down || isEmpty down && isFull upLeft)
                        && not (elem downLeft acc)       -- собрать ходы снизу слева
                    then collectAntMoves downLeft board (downLeft:acc) else acc
    collectDownRight acc = if isEmpty downRight && (isEmpty down  && isFull upRight || isEmpty upRight && isFull down)
                        && not (elem downRight acc)       -- собрать ходы снизу справа
                    then collectAntMoves downRight board (downRight:acc) else acc


-- =========================================
-- Все, что относится к кузнечику
-- =========================================


-- Кузнечик не передвигается общепринятым способом. Он
-- перепрыгивает с одного места на другое незанятое место
-- через фишки улья по прямой линии.Oн должен перепрыгивать как минимум
-- через 1 фишку. 
hopper_cells :: Coord -> Board-> [Coord]
hopper_cells (x,y) board = up ++ down ++ upRight ++ downRight ++ upLeft ++ downLeft
  where
  up = if Map.member (x, y+2) board then takeCell $ for_hopper 1 (x, y) else []      -- направление 1
  down = if Map.member (x, y-2) board then takeCell $ for_hopper 2 (x, y) else []      -- направление 2
  upRight = if Map.member (x+1, y+1) board then takeCell $ for_hopper 3 (x, y) else []      -- направление 3
  downRight = if Map.member (x+1, y-1) board then takeCell $ for_hopper 4 (x, y) else []      -- направление 4
  upLeft = if Map.member (x-1, y+1) board then takeCell $ for_hopper 5 (x, y)  else []     -- направление 5
  downLeft = if Map.member (x-1, y-1) board then takeCell $ for_hopper 6 (x, y) else []      -- направление 6
  takeCell coords       -- первая пустая клетка
    | emptyCells == [] = []
    | otherwise = [head emptyCells]
      where
        emptyCells = filter (\coord -> Map.notMember coord board) coords

-- выдает по номеру список клеток в которым прыгает кузнечик, всего 6 направлений, я их отдельно обрабатываю, собственно из-за этого  и существует  for_hopper
for_hopper :: Int-> Coord -> [Coord]
for_hopper num (x,y)
 |num == 1 =  zip [x,x ..] [y+2, y+4 .. 2*n+2] --список координат y через 2 позиции y > 0
 |num == 2 =  zip [x,x ..] [y-2, y-4 .. -2*n-2] --список координат y через 2 позиции y < 0
 |num == 3 =  zip [x+1, x+2 .. n+1] [y+1, y+2 .. 2*n+2]
 |num == 4 =  zip [x+1, x+2 .. n+1] [y-1, y-2 .. -2*n-2]
 |num == 5 =  zip [x-1, x-2 .. -n-1] [y+1, y+2 .. 2*n+2]
 |num == 6 =  zip [x-1, x-2 .. -n-1] [y-1, y-2 .. -2*n-2]
 |otherwise = []
   where
     n = numberOfPieces
