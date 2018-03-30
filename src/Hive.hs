{-# OPTIONS_GHC -Wall #-}
module Hive where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
--import Data.List(find)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
-- | Запустить игру
runHive :: IO ()
runHive = do
  game <- initGame
  play display bgColor fps game drawGame handleGame updateGame
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 10      -- кол-во кадров в секунду

-- =========================================
-- Модель игры
-- =========================================
-- | Насекомые
data Insect = Queen | Spider | Beetle | Hopper | Ant
  deriving (Show, Eq, Ord)
  
-- | Координаты клетки
type Coord = (Int, Int)

-- | Фишка
type Piece = (Player, Insect, Picture)

-- | Клетка может содержать несколько фишек
type Cell = [Piece]



-- | Поле
type Board = Map Coord Cell

-- | Фишка с координатами
type Movable = (Coord, Piece)

-- | Игрок
data Player = Beige | Black
  deriving (Show, Eq, Ord)

-- | Окончание игры
data Ending = Win Player | Tie

data Step = First |Secind |Third | Fours | Other
  deriving (Eq)
  
-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , gameMovable :: Maybe Movable  -- Nothing - никакая фишка не перемещается, иначе - указана перемещаемая фишка.
  , gameEnding :: Maybe Ending    -- Nothing - игра не окончена.
  }

-- =========================================
-- Инициализация
-- =========================================

-- | Начальное состояние игры
initGame :: IO Game
initGame = gameWithImages <$> loadImages

-- | Инициализировать экран с заданными изображениями
gameWithImages :: [Picture] -> Game
gameWithImages images = Game
  { gameBoard  = Map.union (createCells (-n-1) (-n-1)) (createPieces images)    -- игровое поле - пусто
  , gamePlayer = Beige    -- первый игрок ходит бежевыми
  , gameMovable = Nothing    -- фишка пока что не перемещается
  , gameEnding = Nothing    -- игра не окончена
  }
  where
    n = numberOfPieces
-- | Создаем список из клеток игрового поля
createCells :: Int -> Int -> Board
createCells x y
  | x > n+1 = Map.empty
  | x + y >= 2 * (n+1) = Map.insert (x, y) [] $ createCells (x + 1) ((x+1) - 2 * (n+1))
  | y - x >= 2 * (n+1) = Map.insert (x, y) [] $ createCells (x + 1) (-2 * (n+1) - (x+1))
  | otherwise = Map.insert (x, y) [] (createCells x (y + 2))
  where
    n = numberOfPieces

-- | Создаем список из клеток, в которых вначале находятся фишки
createPieces :: [Picture] -> Board
createPieces pic = Map.fromList
  [ ((-x, -10), [(Beige, Queen, t 0)])
  , ((-x, -8), [(Beige, Spider, t 1)])
  , ((-x, -6), [(Beige, Spider, t 1)])
  , ((-x, -4), [(Beige, Beetle, t 2)])
  , ((-x, -2), [(Beige, Beetle, t 2)])
  , ((-x, 0), [(Beige, Hopper, t 3)])
  , ((-x, 2), [(Beige, Hopper, t 3)])
  , ((-x, 4), [(Beige, Hopper, t 3)])
  , ((-x, 6), [(Beige, Ant, t 4)])
  , ((-x, 8), [(Beige, Ant, t 4)])
  , ((-x, 10), [(Beige, Ant, t 4)])

  , ((x, -10), [(Black, Queen, t 5)])
  , ((x, -8), [(Black, Spider, t 6)])
  , ((x, -6), [(Black, Spider, t 6)])
  , ((x, -4), [(Black, Beetle, t 7)])
  , ((x, -2), [(Black, Beetle, t 7)])
  , ((x, 0), [(Black, Hopper, t 8)])
  , ((x, 2), [(Black, Hopper, t 8)])
  , ((x, 4), [(Black, Hopper, t 8)])
  , ((x, 6), [(Black, Ant, t 9)])
  , ((x, 8), [(Black, Ant, t 9)])
  , ((x, 10), [(Black, Ant, t 9)])]
  where
    x = cellDistance + numberOfPieces + 1
    t = takePic pic

    -- Взять картинку из списка по номеру (кажется, такой подход абсолютно отвратителен, но я не уверена)
    takePic :: [Picture] -> Int -> Picture
    takePic [] _ = blank
    takePic (p : ps) n
      | n < 0 = blank
      | n == 0 = p
      | otherwise = takePic ps (n - 1)


-- =========================================
-- Загрузка изображений
-- =========================================


-- | Загрузка изображения фишки в нужном масштабе.
loadPieceImage :: String -> IO Picture
loadPieceImage s = fmap (translate 0 0 . scale k k)
  (maybePicToPic <$> (loadJuicyPNG path))
  where
    path = "images/" ++ s ++ ".png"
    k = 6 / 5 * fromIntegral cellSizeX / (fromIntegral pieceWidth)

    -- | Переводит Maybe Picture в Picture
    maybePicToPic :: Maybe Picture -> Picture
    maybePicToPic (Just p) = p
    maybePicToPic Nothing = blank


-- | Загрузка изображений всех фишек в нужном масштабе.
loadImages :: IO [Picture]
loadImages = listToIO $ loadPieceImage <$> allImageNames
  where
    -- | Переводит список IO a в IO списка a
    listToIO :: [IO a] -> IO [a]
    listToIO [] = return []
    listToIO (x:xs) = do
      y <- x
      fmap (y:) (listToIO xs)


-- =========================================
-- Отрисовка игры
-- =========================================


-- | Рисуем всё
drawGame :: Game -> Picture
drawGame Game{gameBoard = board, gameEnding = maybeEnding} = pictures
  [ drawAllCells board
  , drawAllInsects board
  , drawEnding maybeEnding]

-- | Рисуем все клетки
drawAllCells :: Board -> Picture
drawAllCells board = scale cx cy $ pictures $ map drawCell tl
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY
    tl = Map.toList board

-- | Рисуем клетку
drawCell :: (Coord, Cell) -> Picture
drawCell ((x, y),_) = color  black $ line
  [ (a - 1 / 3, b - 1)
  , (a + 1 / 3, b - 1)
  , (a + 2 / 3, b)
  , (a + 1 / 3, b + 1)
  , (a - 1 / 3, b + 1)
  , (a - 2 / 3, b)
  , (a - 1 / 3, b - 1)]
  where 
    a = fromIntegral x
    b = fromIntegral y

-- | Рисуем всех насекомых
drawAllInsects :: Board -> Picture
drawAllInsects board = pictures(map drawInsect tl)
  where
    tl = Map.toList board

-- | Рисуем самое верхнее насекомое в клетке
drawInsect :: (Coord, Cell) -> Picture
drawInsect (_, []) = blank
drawInsect ((x, y), ((_, _, pic):_)) =
  translate kx ky pic
  where
    kx = fromIntegral (cellSizeX * x)
    ky = fromIntegral (cellSizeY * y)

-- | Рисуем конец игры
drawEnding :: Maybe Ending -> Picture
drawEnding Nothing = blank
drawEnding (Just ending) = placeText $ text $ endingText ending
  where
    placeText = (translate (fromIntegral (- screenWidth) / 2 + 20) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.4 0.4

-- | Надпись об окончании игры
endingText :: Ending -> String
endingText Tie = "It's a Tie:)"
endingText (Win Black) = "Black Team Won"
endingText (Win Beige) = "Beige Team Won"

-- =========================================
-- Обработка событий
-- =========================================


-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ shiftGame $ 
        makeMove (mouseToCell mouse (gameBoard game)) game    -- фишка уже взята
handleGame (EventKey (MouseButton RightButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ shiftGame $ 
        makeMove (mouseToCell mouse (gameBoard game)) game    -- фишка уже взята
handleGame _ game = game


-- | Взять фишку с координатами под мышкой, если возможно
takePiece :: Point -> Game -> Game
takePiece (x, y) game@Game{gamePlayer = player, gameBoard = board}
  | pieces == [] = game
  | pieceColor top /= player = game
  | possibleMoves movable board == [] = game
  | otherwise = Game
    { gameBoard = deleteInsect (i, j) board
    , gamePlayer = player
    , gameMovable = Just movable
    , gameEnding = Nothing
    }
  where
    i = round (x / fromIntegral cellSizeX)
    j = round (y / fromIntegral cellSizeY)
    pieces = fromMaybe [] $ Map.lookup (i, j) board    -- список фишек в клетке с нужными координатами
    top = head pieces    -- самая верхняя фишка в списке
    movable = ((i, j), top)
    pieceColor (p, _, _) = p


-- | Удаление фишки из старой позиции (перед перемещением)
deleteInsect :: Coord -> Board -> Board
deleteInsect (i, j) board
  | pieces == [] = Map.delete (i, j) board
  | otherwise = Map.adjust tail (i, j) board
  where
    pieces = fromMaybe [] $ Map.lookup (i, j) board    -- список фишек в клетке с нужными координатами

-- | Получить клетку под мышкой (в которую хотим поставить фишку).
mouseToCell :: Point -> Board -> Maybe Coord
mouseToCell (x, y) board
  | isNothing l = Nothing
  | otherwise = Just (i, j)
  where
    i = round (x / fromIntegral cellSizeX)
    j = round (y / fromIntegral cellSizeY)
    l = Map.lookup (i, j) board

-- | Сделать ход, если возможно
makeMove :: Maybe Coord -> Game -> Game
makeMove Nothing game = game    -- если ткнули не в клетку поля
makeMove (Just (i, j)) game@Game{gamePlayer = player, gameBoard = board, gameMovable = Just movable}
   | elem (i, j) (possibleMoves movable board) = Game    -- если выбранный ход возможен
     { gamePlayer = switchPlayer player
     , gameBoard = putInsect (snd movable) (i,j) board
     , gameMovable = Nothing
     , gameEnding = Nothing
   }
   | otherwise = game    -- если выбранный ход невозможен
makeMove _ game = game    -- это просто так, чтобы компилятор не ругался

-- | Поставить фишку
putInsect :: Piece -> Coord -> Board -> Board
putInsect piece = Map.adjust (piece:)

-- | Список координат всех допустимых клеток для постановки фишки (В ПРОЦЕССЕ НАПИСАНИЯ)
possibleMoves ::Movable-> Board ->  [Coord]
possibleMoves ( (x,y), (_,ins,_)) board  -- flag true если мы двигаем фишку из началаьной позиции (со "старта"), иначе false, 
                                       -- в случае старта должно возвратить список всех клеток поля             
  | is_not_possible == True && ins /= Hopper && ins /= Beetle && flag == False  = [(x,y)]
  | flag == False && ins == Queen  = (x,y) : queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList only_free_cells)) 
  | flag == False && ins == Beetle = (x,y) : queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList board))
  | flag == False && ins == Hopper = (x,y) : delStartCells (hopper_cells (x,y) board) 
  | otherwise =  delStartCells (map fst $ Map.toList only_free_cells)
 where
  flag = x < -(n+1) || x > n+1
  n = numberOfPieces
  only_free_cells = Map.filterWithKey (\_ val -> val == []) board
  is_not_possible = poss_move board (x,y)  

-- | Может ли двигаться данная фишка, true - не может двигаться,false иначе
poss_move :: Board -> Coord -> Bool
poss_move board (x, y)    
  | sum [isNotEmpty (x-1, y+1), isNotEmpty (x+1, y+1),isNotEmpty (x-1, y-1) , isNotEmpty (x+1, y-1), isNotEmpty (x, y+2) , isNotEmpty (x, y-2)] == 5   = True
  | otherwise =  check1(x,y) || check2(x,y)  -- проверка на расположение 3 фишек вокруг данной,всего 2 конфигурации                         
 where
  isNotEmpty (i, j) = if Map.lookup (i, j) board /= (Just []) then 1 else 0
  check1 (i,j) = if  Map.lookup (i, j+2) board /= (Just []) && Map.lookup (i-1, j-1) board /= (Just []) && Map.lookup (i+1, j-1) board /= (Just []) then True else False
  check2 (i,j) = if  Map.lookup (i, j-2) board /= (Just []) && Map.lookup (i-1, j+1) board /= (Just []) && Map.lookup (i+1, j+1) board /= (Just []) then True else False  

-- | удаляет из списка координат стартовые клетки
delStartCells :: [Coord] -> [Coord]
delStartCells [] = []
delStartCells l = filter (\(x,_) -> x >= -(n+1) && x <= n+1 ) l
 where n = numberOfPieces


-- | координаты для королевы и жука
-- |Пчеломатка может перемещаться всего на 1 "клетку". Жук, также как и пчеломатка, может перемещаться только на 1 позицию за
-- |ход. Но в отличии от всех остальных фишек, он может перемещать поверх других фишек.
queen_beetle_cells :: Coord -> [Coord] -> [Coord]
queen_beetle_cells (x,y) = filter (\(a,b) -> (a,b) == (x-1, y+1) 
  ||  (a,b) == (x+1,y+1)
  ||  (a,b) == (x,y+2)
  ||  (a,b) == (x,y-2)
  ||  (a,b) == (x-1,y-1)
  ||  (a,b) == (x+1,y-1)
   ) 


-- на вход поле и список ключей выдает, поле с клетками   по данным ключам 
keysToBoard :: [Coord] -> Board -> Board 
keysToBoard [] _ = Map.empty
keysToBoard (point: xs) board 
  | Map.toList board == [] = Map.empty
  | Map.member point board = Map.insert point listForPoint (keysToBoard xs board)
  | otherwise = keysToBoard xs board 
  where 
   listForPoint = maybePiecetoPiece (Map.lookup point board)


maybePiecetoPiece :: Maybe Cell -> Cell
maybePiecetoPiece (Just l) = l
maybePiecetoPiece Nothing = []

-- Кузнечик не передвигается общепринятым способом. Он
-- перепрыгивает с одного места на другое незанятое место
-- через фишки улья по прямой линии.Oн должен перепрыгивать как минимум
-- через 1 фишку. 
hopper_cells :: Coord -> Board-> [Coord]
hopper_cells (x,y) board 
  | l == [] = []
  -- ищем в каждом направлении первые n непустых клеток
  -- фльтром получаем из данного списка клеток  - l координаты клеток в одном из 6 направлений, 
  -- после по этим координатам достаем поле(уже вместе со списками насекомых)
  -- dir1, dir3, dir4 идут по порядку следования, dir2 dir5 dir6 наоборот, поэтому делаем reverse 
  | otherwise =  delOneCoord (x,y+2) (takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir1 )l) board) False) ++
                 delOneCoord (x,y-2) (takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir2 )l) board) True)  ++
                 delOneCoord (x+1,y+1)(takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir3 )l) board) False) ++ 
                 delOneCoord (x+1,y-1)(takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir4 )l) board) False) ++
                 delOneCoord (x-1,y+1)(takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir5 )l) board) True) ++
                 delOneCoord (x-1,y-1)(takeWhileMap (keysToBoard (filter (\(a,b) -> elem (a,b) dir6 )l) board) True) 
  where
  l = map fst $ Map.toList board
  dir1 = (for_hopper 1 (x,y) (maxmin l)) --направление 1(direction)
  dir2 = (for_hopper 2 (x,y) (maxmin l)) --направление 2
  dir3 = (for_hopper 3 (x,y) (maxmin l)) --направление 3                             
  dir4 = (for_hopper 4 (x,y) (maxmin l)) --направление 4                               
  dir5 = (for_hopper 5 (x,y) (maxmin l)) --направление 5
  dir6 = (for_hopper 6 (x,y) (maxmin l)) --направление 6 

--вспомогательные функции
delOneCoord :: Coord -> [Coord] -> [Coord]
delOneCoord _ [] = []
delOneCoord a (x:xs) 
  | a == x = xs
  | otherwise = x : delOneCoord a xs 

maxmin:: [Coord] -> [Coord]
maxmin [] = []
maxmin l  = [  ( maximum (map fst $ l), maximum (map snd $ l) ) , ( minimum (map fst $ l),minimum (map snd $ l))]  
 
 -- Возвращает свободные клетки до первой занятой
takeWhileMap :: Board -> Bool->[Coord] 
takeWhileMap board flag  
  | l == [] = []
  | snd head_l == [] =  fst head_l : (takeWhileMap tail_map flag)
  | otherwise = [] 
  where 
    l = if flag then reverse (Map.toList board) else (Map.toList board) 
    head_l = head l 
    tail_map = Map.fromList (tail l)

-- выдает по номеру список клеток в которым прыгает кузнечик, всего 6 направлений, я их отдельно обрабатываю, собственно из-за этого  и существует  for_hopper
for_hopper :: Int-> Coord -> [Coord] -> [Coord]
for_hopper _ _ [] = []
for_hopper n (x,y) [(max_x,max_y),(min_x,min_y)]
 | n > 6 || n < 1 = [] 
 |n == 1 =  zip [x,x ..] [y+2, y+4 .. max_y] --список координат y через 2 позиции y > 0
 |n == 2 =  zip [x,x ..] [y-2, y-4 .. min_y] --списко координат y через 2 позиции y < 0
 |n == 3 =  zip [x+1,x+2 .. max_x] [y+1,y+2 .. max_y]
 |n == 4 =  zip [x+1,x+2 .. max_x] [y-1,y-2 .. min_y]
 |n == 5 =  zip [x-1,x-2 .. min_x] [y+1,y+2 .. max_y]
 |n == 6 =  zip [x-1,x-2 .. min_x] [y-1,y-2 .. min_y]
 |otherwise = []

--for_hopper _ _ [] = []

  -- | Установить gameEnding в Game, если игра завершилась
-- Кузнечик не передвигается общепринятым способом. Он
-- перепрыгивает с одного места на другое незанятое место
-- через фишки улья по прямой линии.Oн должен перепрыгивать как минимум
-- через 1 фишку.

-- | Установить gameEnding в Game, если игра завершилась
checkWinner :: Game -> Game
checkWinner game = game{gameEnding = winner (gameBoard game)}

-- | Сменить текущего игрока
switchPlayer :: Player -> Player
switchPlayer Beige = Black
switchPlayer Black = Beige

-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- | Определение победителя
winner :: Board -> Maybe Ending
winner board
  | blackLose && beigeLose = Just Tie
  | blackLose = Just (Win Beige)
  | beigeLose = Just (Win Black)
  | otherwise = Nothing
  where
    blackLose = fromMaybe False $ beeIsLocked board <$> (beeCoord Black)     -- черная пчела окружена
    beigeLose = fromMaybe False $ beeIsLocked board <$> (beeCoord Beige)    -- бежевая пчела окружена
    beeCoord :: Player -> Maybe Coord       -- координаты пчелы данного цвета
    beeCoord player = takeCoord $ Map.filter hasBee board
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
      isNotEmpty (i, j) = Map.lookup (i, j) board /= (Just [])

-- | Это просто для вызова shiftBoard,
-- потому что делать shiftBoard еще больше я замучаюсь
shiftGame  :: Game -> Game
shiftGame game@Game{gameBoard = board} = game{gameBoard = shiftBoard board}
-- | Передвинуть массив фишек, если он касается края поля
-- если поставить фишки на противоположные края, то будет очень плохо
-- но предполагается, что у нас будет possibleMoves, так что все нормально
shiftBoard :: Board -> Board
shiftBoard board    -- закройте глазки и не смотрите на этот ужас
  | Map.filterWithKey (\(x, _) a -> x == -(n+1) && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\y -> ((-(n+1), y), [])) [-(n+1),-(n-1)..n+1]) $
            Map.union (Map.fromList $
              map (\x -> ((x, -2*(n+1)-x), [])) [-(n+1)..0]) $
                Map.filterWithKey (\(x, y) _ -> x+y /= 2*(n+2) && x /= n+2 || isSide x) $
                  shiftCoord (1, 1) board
  | Map.filterWithKey (\(x, y) a -> x >= -(n+1) && y-x == 2*(n+1) && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\y -> ((-(n+1), y), [])) [-(n+1),-(n-1)..n+1]) $
            Map.union (Map.fromList $
              map (\x -> ((x, 2*(n+1)+x), [])) [-(n+1)..0]) $
                Map.filterWithKey (\(x, y) _ -> x-y /= 2*(n+2) && x /= n+2 || isSide x) $
                  shiftCoord (1, -1) board
  | Map.filterWithKey (\(x, y) a -> x <= n+1 && x+y == 2*(n+1) && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\x -> ((x, 2*(n+1)-x), [])) [0..n+1]) $
            Map.union (Map.fromList $
              map (\x -> ((x, 2*(n+1)+x), [])) [-(n+1)..0]) $
                Map.filterWithKey (\(x, y) _ -> x+y /= -2*(n+2) && x-y /= 2*(n+2) || isSide x) $
                  shiftCoord (0, -2) board
  | Map.filterWithKey (\(x, _) a -> x == n+1 && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\x -> ((x, 2*(n+1)-x), [])) [0..n+1]) $
            Map.union (Map.fromList $
              map (\y -> ((n+1, y), [])) [n+1,n-1.. -(n+1)]) $
                Map.filterWithKey (\(x, y) _ -> x+y /= -2*(n+2) && x /= -(n+2) || isSide x) $
                  shiftCoord (-1, -1) board
  | Map.filterWithKey (\(x, y) a -> x <= n+1 && x-y == 2*(n+1) && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\x -> ((x, x-2*(n+1)), [])) [0..n+1]) $
            Map.union (Map.fromList $
              map (\y -> ((n+1, y), [])) [n+1,n-1.. -(n+1)]) $
                Map.filterWithKey (\(x, y) _ -> y-x /= 2*(n+2) && x /= -(n+2) || isSide x) $
                  shiftCoord (-1, 1) board
  | Map.filterWithKey (\(x, y) a -> x >= -(n+1) && x+y == -2*(n+1) && a/=[]) board /= Map.empty
      = shiftBoard $
        Map.union (Map.fromList $
          map (\x -> ((x, x-2*(n+1)), [])) [0..n+1]) $
            Map.union (Map.fromList $
              map (\x -> ((x, -2*(n+1)-x), [])) [-(n+1)..0]) $
                Map.filterWithKey (\(x, y) _ -> y-x /= 2*(n+2) && x+y /= 2*(n+2) || isSide x) $
                  shiftCoord (0, 2) board
  | otherwise = board
    where
      n = numberOfPieces
      isSide x = x > n+2 || x < -(n+2)
      shiftCoord :: (Int, Int) -> Board -> Board    -- сдвигает поле на (i, j)
      shiftCoord (i, j) = Map.mapKeys (\(x, y) -> if (x>= -(n+1)) && (x<= n+1)
                                                    then(x+i, y+j)
                                                    else (x, y))

-- =========================================
-- Константы, параметры игры
-- =========================================


-- | Количество фишек у каждого игрока
numberOfPieces :: Int
numberOfPieces = 11

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 2 * (numberOfPieces + 1 + cellDistance) + 2

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 4 * (numberOfPieces + 1) + 3

-- | Ширина одной клетки в пикселях.
cellSizeX :: Int
cellSizeX = 35

-- | Высота одной клетки в пикселях.
cellSizeY :: Int
cellSizeY = round ((fromIntegral cellSizeX) / ( sqrt 3) :: Double)

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSizeX * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSizeY * boardHeight

-- | Расстояние от игрового поля до фишек в начале игры, выраженное в клетках
cellDistance :: Int
cellDistance = 4

-- Ширина изображений фишек в пикселях
pieceWidth :: Int
pieceWidth = 500

-- | Список всех имен изображений
allImageNames :: [String]
allImageNames = fmap (++)
  (show <$> [Beige, Black]) <*>
  (show <$> [Queen, Spider, Beetle, Hopper, Ant])

-- | Список координат, где стоят фишки в начале игры
coordsOnStart :: [Coord]
coordsOnStart = [ (-15, -10),(-15, -8), (-15, -6),(-15, -4),(-15, -2), 
                  (-15, 0),  (-15, 2),  (-15, 4), (-15, 6), (-15, 8), (-15, 10),
                  (15, 10),  (15, 8),   (15, 6),  (15, 4),  (15, 2), 
                  (15, 0),   (15, -2),  (15, -4), (15, -6), (15, -8), (15, -10)] 

