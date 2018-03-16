module Hive where


import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.List()
import Data.Map (Map)
import qualified Data.Map as Map

-- | Запустить игру
runHive :: IO ()
runHive = do
  game <- initGame
  play display bgColor fps game drawGame handleGame updateGame
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 0      -- кол-во кадров в секунду


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

---- | Клетка
--data Cell = Cell
--  { xx :: Int    -- x-координата
--  , yy :: Int    -- y-координата
--  , insects :: [Piece]    -- Насекомые в клетке
--  }
--  deriving (Eq)
  
-- | Игрок				
data Player = Beige | Black deriving (Show, Eq, Ord)

---- | Поле
--type Board = [Cell]

-- | Поле
type Board = Map Coord Cell

-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , gameWinner :: Maybe Player    -- Победитель.
  }

-- =========================================
-- Инициализация
-- =========================================


-- | Начальное состояние игры.
-- Пробуем загрузить изображения
initGame :: IO Game
initGame = gameWithImages <$> loadImages


  -- | Инициализировать экран с заданными изображениями.
  -- Игровое поле — пусто.
  -- Первый игрок ходит бежевыми.
gameWithImages :: [Picture] -> Game
gameWithImages images = Game
  { gameBoard  = Map.union (createCells (-11) (-11)) (createPieces images)
  , gamePlayer = Beige
  , gameWinner = Nothing
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
 
    -- Взять Картинку из списка по номеру	
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
loadImages = listToIO (loadPieceImage <$> allImageNames)
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


-- | Рисуем игровое поле.
drawGame :: Game -> Picture
drawGame (Game{gameBoard = board}) = pictures
  [ drawAllCells board
  , drawAllInsects board]

-- | Рисуем все клетки
drawAllCells :: Board -> Picture
drawAllCells board = scale cx cy (pictures (map drawCell tl))
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY
    tl = Map.toList board


-- | Рисуем клетку
drawCell :: (Coord, Cell) -> Picture
drawCell ((x, y),_) = color  black (line
  [ (a - 1 / 3, b - 1)
  , (a + 1 / 3, b - 1)
  , (a + 2 / 3, b)
  , (a + 1 / 3, b + 1)
  , (a - 1 / 3, b + 1)
  , (a - 2 / 3, b)
  , (a - 1 / 3, b - 1)])
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


-- | Обработка событий.
handleGame :: Event -> Game -> Game
--handleGame (EventKey (MouseButton LeftButton) Down _ mouse) = placeMark (mouseToCell mouse)
handleGame _ = id


-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id


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
