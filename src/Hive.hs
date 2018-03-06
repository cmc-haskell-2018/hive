module Hive where

import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру
runHive :: IO ()
runHive = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
    bgColor = white   -- цвет фона
    fps     = 0      -- кол-во кадров в секунду

-- =========================================
-- Модель игры
-- =========================================

-- | Насекомые
data Insect = Queen | Spider | Beetle | Hopper | Ant
  deriving (Eq, Show)

-- | Клетка
data Cell = Cell
  { x :: Int    -- x-координата
  , y :: Int    -- y-координата
  , insects :: [(Player,Insect)]    -- Насекомые в клетке
  }

-- | Игрок				
data Player = Black | Beige

-- | Поле
type Board = [Cell]

-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , gameWinner :: Maybe Player    -- Победитель.
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит бежевыми.
initGame :: Game
initGame = Game
  { gameBoard  = createCells (-11) (-11) ++ createPieces
  , gamePlayer = Beige
  , gameWinner = Nothing
  }

-- | Создаем список из клеток игрового поля
createCells :: Int -> Int -> Board
createCells xx yy
  | xx > n = []
  | xx + yy >= 2 * n = this : createCells (xx + 1) (xx + 1 - 2 * n)
  | yy - xx >= 2 * n = this : createCells (xx + 1) (- 2 * n - xx - 1)
  | otherwise = this : createCells xx (yy + 2)
  where
    this = Cell { x = xx, y = yy, insects = []}
    n = numberOfPieces

-- | Создаем список из клеток, в которых вначале находятся фишки
createPieces :: Board
createPieces =
  [ Cell{x = -xx, y = -10, insects = [(Beige, Queen)]}
  , Cell{x = -xx, y = -8, insects = [(Beige, Spider)]}
  , Cell{x = -xx, y = -6, insects = [(Beige, Spider)]}
  , Cell{x = -xx, y = -4, insects = [(Beige, Beetle)]}
  , Cell{x = -xx, y = -2, insects = [(Beige, Beetle)]}
  , Cell{x = -xx, y = 0, insects = [(Beige, Hopper)]}
  , Cell{x = -xx, y = 2, insects = [(Beige, Hopper)]}
  , Cell{x = -xx, y = 4, insects = [(Beige, Hopper)]}
  , Cell{x = -xx, y = 6, insects = [(Beige, Ant)]}
  , Cell{x = -xx, y = 8, insects = [(Beige, Ant)]}
  , Cell{x = -xx, y = 10, insects = [(Beige, Ant)]}

  , Cell{x = xx, y = -10, insects = [(Black, Queen)]}
  , Cell{x = xx, y = -8, insects = [(Black, Spider)]}
  , Cell{x = xx, y = -6, insects = [(Black, Spider)]}
  , Cell{x = xx, y = -4, insects = [(Black, Beetle)]}
  , Cell{x = xx, y = -2, insects = [(Black, Beetle)]}
  , Cell{x = xx, y = 0, insects = [(Black, Hopper)]}
  , Cell{x = xx, y = 2, insects = [(Black, Hopper)]}
  , Cell{x = xx, y = 4, insects = [(Black, Hopper)]}
  , Cell{x = xx, y = 6, insects = [(Black, Ant)]}
  , Cell{x = xx, y = 8, insects = [(Black, Ant)]}
  , Cell{x = xx, y = 10, insects = [(Black, Ant)]}]
  where
    xx = cellDistance + numberOfPieces
  
-- =========================================
-- Отрисовка игры
-- =========================================

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame (Game{gameBoard = board}) = (scale cx cy (drawBoard board))
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY

-- | Нарисовать поле
drawBoard :: Board -> Picture
drawBoard board = pictures (map drawCell board)

-- | Нарисовать клетку
drawCell :: Cell -> Picture
drawCell (Cell {x = xx, y = yy}) = line
  [ (a - 1 / 3, b - 1)
  , (a + 1 / 3, b - 1)
  , (a + 2 / 3, b)
  , (a + 1 / 3, b + 1)
  , (a - 1 / 3, b + 1)
  , (a - 2 / 3, b)
  , (a - 1 / 3, b - 1)]
  where 
    a = fromIntegral xx
    b = fromIntegral yy

-- =========================================
-- Обработка событий
-- =========================================

-- | Обработка событий.
handleGame :: Event -> Game -> Game
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
