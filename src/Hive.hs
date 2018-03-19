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
    , gameEnding = Nothing}
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
    , gameBoard = putInsect (snd movable) (i, j) board
    , gameMovable = Nothing
    , gameEnding = Nothing}
  | otherwise = game    -- если выбранный ход невозможен
makeMove _ game = game    -- это просто так, чтобы компилятор не ругался

-- | Поставить фишку
putInsect :: Piece -> Coord -> Board -> Board
putInsect piece = Map.adjust (piece:)

-- | Список координат всех допустимых клеток для постановки фишки - НУЖНО НАПИСАТЬ!!!
-- Пока что возвращает координаты всех клеток поля
possibleMoves :: Movable -> Board -> [Coord]
possibleMoves _ board = map fst $ Map.toList board

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
