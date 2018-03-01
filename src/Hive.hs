module Hive where

import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру
runHive :: IO ()
runHive = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Улей" (screenWidth, screenHeight) (0, 0)
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

-- | Игровое поле
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
  { gameBoard  = createCells 0 11
  , gamePlayer = Beige
  , gameWinner = Nothing
  }

-- | Создаем список из всех клеток поля
createCells :: Int -> Int -> Board
createCells xx yy
  | xx > 22 = []
  | xx + yy >= 55 = this : createCells (xx + 1) (xx - 10)
  | yy - xx >= 33 = this : createCells (xx + 1) (10 - xx)
  | otherwise = this : createCells xx (yy + 2)
  where
    this = Cell { x = xx, y = yy, insects = []}

-- =========================================
-- Отрисовка игры
-- =========================================

-- | Отобразить игровое поле.
drawGame :: Game -> Picture
drawGame (Game{gameBoard = board}) = translate (-w) (-h) (scale cx cy (drawBoard board))
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать поле
drawBoard :: Board -> Picture
drawBoard board = pictures (map drawCell board)

-- | Нарисовать клетку
drawCell :: Cell -> Picture
drawCell (Cell {x = xx, y = yy}) = line
  [ (a + 1 / 3, b)
  , (a + 1, b)
  , (a + 4 / 3, b + 1)
  , (a + 1, b + 2)
  , (a + 1 / 3, b + 2)
  , (a, b + 1)
  , (a + 1 / 3, b)]
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
boardWidth  = 2 * numberOfPieces + 2

-- | Высота игрового поля в клетках.
boardHeight :: Int
boardHeight = 4 * numberOfPieces + 2

-- | Ширина одной клетки в пикселях.
cellSizeX :: Int
cellSizeX = 30

-- | Высота одной клетки в пикселях.
cellSizeY :: Int
cellSizeY = round ((fromIntegral cellSizeX) / (sqrt 3))

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSizeX * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSizeY * boardHeight
