module Hive.Config
  where

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
cellSizeX = 30

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
