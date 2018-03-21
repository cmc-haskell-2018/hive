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
  { gameBoard  = Map.union (createCells (-11) (-11)) (createPieces images)    -- игровое поле - пусто
  , gamePlayer = Beige    -- первый игрок ходит бежевыми
  , gameMovable = Nothing    -- фишка пока что не перемещается
  , gameEnding = Nothing    -- игра не окончена
  }
  
-- | Создаем список из клеток игрового поля
createCells :: Int -> Int -> Board
createCells x y
  | x > n = Map.empty
  | x + y >= 2 * n = Map.insert (x, y) [] (createCells (x + 1) (x + 1 - 2 * n))
  | y - x >= 2 * n = Map.insert (x, y) [] (createCells (x + 1) (- 2 * n - x - 1))
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
    x = cellDistance + numberOfPieces
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
drawGame Game{gameBoard = board} = pictures
  [ drawAllCells board
  , drawAllInsects board]

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


-- =========================================
-- Обработка событий
-- =========================================


-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ makeMove (mouseToCell mouse (gameBoard game)) game    -- фишка уже взята
handleGame (EventKey (MouseButton RightButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ makeMove (mouseToCell mouse (gameBoard game)) game    -- фишка уже взята
handleGame _ game = game

--------------------------------------------------------------------------------------------------------------------------------------
-- | Взять фишку с координатами под мышкой, если возможно
takePiece :: Point -> Game -> Game
takePiece (x, y) game@Game{gamePlayer = player, gameBoard = board}
  | pieces == [] = game
  | pieceColor top /= player = game
  -- possibleMoves movable board False  == [] = game -- кажется это проверка здесь не нужна, 
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
  where
makeMove _ game = game    -- это просто так, чтобы компилятор не ругался
  
-- | Поставить фишку
putInsect :: Piece -> Coord -> Board -> Board
putInsect piece = Map.adjust (piece:)

-- | Список координат всех допустимых клеток для постановки фишки - НУЖНО НАПИСАТЬ!!!
-- Пока что возвращает координаты всех клеток поля
-- possibleMoves _ board = map fst $ Map.toList board

possibleMoves ::Movable-> Board ->  [Coord]
possibleMoves ( (x,y), (_,ins,_)) board  -- flag true если мы двигаем фишку из началаьной позиции (со "старта"), иначе false, 
                                       -- в случае старта должно возвратить список всех клеток поля             
  | is_not_possible == True && ins /= Hopper && ins /= Beetle && flag == False  = [(x,y)]
  | flag == False && ins == Queen  = queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList only_free_cells)) 
  | flag == False && ins == Beetle = queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList board))
  | flag == False && ins == Hopper = hopper_cells(x,y)        (delStartCells (map fst $ Map.toList only_free_cells)) 
  | otherwise =  delStartCells (map fst $ Map.toList only_free_cells)
 where
  flag = elem (x,y) coordsOnStart
  only_free_cells = Map.filterWithKey (\_ val -> val == []) board
  is_not_possible = poss_move board (x,y)  

-- | Может ли двигаться данная фишка, true - не может двигаться
poss_move :: Board -> Coord -> Bool
poss_move board (x, y) =   (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2) ==False)
                        
                        || (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2)== False && isNotEmpty (x, y-2))
                        
                        || (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) == False &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2) )
                        
                        || (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1)== False && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2))
                        
                        || (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1)== False &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2))
                        
                        || (isNotEmpty (x-1, y+1) == False && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2))

                        || (isNotEmpty (x-1, y+1) && isNotEmpty (x+1, y+1) &&
                           isNotEmpty (x-1, y-1) && isNotEmpty (x+1, y-1) &&
                           isNotEmpty (x, y+2) && isNotEmpty (x, y-2))
 where
  isNotEmpty (i, j) = Map.lookup (i, j) board /= (Just [])

-- | удаляет из списка координат стартовые клетки
delStartCells :: [Coord] -> [Coord]
delStartCells [] = []
delStartCells l = filter (\(a,b) -> elem (a,b) coordsOnStart == False ) l

-- | координаты для королевы и жука
-- |Пчеломатка может перемещаться всего на 1 "клетку". Жук, также как и пчеломатка, может перемещаться только на 1 позицию за
-- |ход. Но в отличии от всех остальных фишек, он может перемещать поверх других фишек.
queen_beetle_cells :: Coord -> [Coord] -> [Coord]
queen_beetle_cells _ [] = []
queen_beetle_cells (x,y) l = filter (\(a,b) -> (a,b) == (x-1, y+1) 
  ||  (a,b) == (x+1,y+1)
  ||  (a,b) == (x,y+2)
  ||  (a,b) == (x,y-2)
  ||  (a,b) == (x-1,y-1)
  ||  (a,b) == (x+1,y-1)
  ) l
-- Кузнечик не передвигается общепринятым способом. Он
-- перепрыгивает с одного места на другое незанятое место
-- через фишки улья по прямой линии.Oн должен перепрыгивать как минимум
-- через 1 фишку.

hopper_cells :: Coord -> [Coord] -> [Coord]
hopper_cells _ [] = []
hopper_cells (x,y) l = filter (\(a,b) -> 
  -- выбираются  клетки для вертикальных прыжков
     elem (a,b) (zip [x,x ..] list_ver1 ) 
  || elem (a,b) (zip [x,x ..] list_ver2 ) 
  -- далее выбираются ячекйки для диагональных прыжков кузнечика 
  || elem (a,b) (zip list_x1 list_y1) 
  || elem (a,b) (zip list_x1 list_y2)
  || elem (a,b) (zip list_x2 list_y1) 
  || elem (a,b) (zip list_x2 list_y2) 
    ) l
 where
  list_ver1 = [y+4, y+6 .. maximum (map snd $ l)] --список координат y через 2 позиции y > 0
  list_ver2 = [y-4, y-6 .. minimum (map snd $ l)] --списко координат y через 2 позиции y < 0
  list_x1 = [x+2,x+3 .. maximum (map fst $ l)] --список координат х > 0 
  list_x2 = [x-2,x-3 .. minimum (map fst $ l)] --список координат x < 0
  list_y1 = [y+2,y+3 .. maximum (map snd $ l)] -- список координат y > 0 
  list_y2 = [y-2,y-3 .. minimum (map snd $ l)] -- список координат y < 0 


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

-- | Определение победителя - НУЖНО НАПИСАТЬ!!!
winner :: Board -> Maybe Ending
winner _ = Nothing -- Nothing - никто пока не выйграл => добвить проверку на условия победы и ничьей


-- =========================================
-- Константы, параметры игры
-- =========================================


-- | Количество фишек у каждого игрока
numberOfPieces :: Int
numberOfPieces = 11

-- | Ширина игрового поля в клетках.
boardWidth :: Int
boardWidth  = 2 * (numberOfPieces + cellDistance) + 2

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 4 * numberOfPieces + 3

-- | Ширина одной клетки в пикселях.
cellSizeX :: Int
cellSizeX = 35

-- | Высота одной клетки в пикселях.
cellSizeY :: Int
cellSizeY = round ((fromIntegral cellSizeX) / (sqrt 3))

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

