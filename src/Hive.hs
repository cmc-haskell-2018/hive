module Hive where


import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.List(find)


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

-- | Фишка
type Piece = (Player, Insect, Picture)

-- | Клетка
data Cell = Cell
  { xx :: Int    -- x-координата
  , yy :: Int    -- y-координата
  , insects :: [Piece]    -- Насекомые в клетке
  }
  deriving (Eq)

  
-- | Игрок				
data Player = Beige | Black
  deriving (Show, Eq, Ord)

-- | Поле
type Board = [Cell]

-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , movablePieceInGame :: Maybe Piece  -- Nothing никакая фишка не перемещается, иначе указана перемещаемая фишка
  }

data GameEnding = Win Player | Tie
  
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
  { gameBoard  = createCells (-11) (-11) ++ createPieces images
  , gamePlayer = Beige
  , movablePieceInGame = Nothing
  }
-- | Создаем список из клеток игрового поля
createCells :: Int -> Int -> Board
createCells x y
  | x > n = []
  | x + y >= 2 * n = this : createCells (x + 1) (x + 1 - 2 * n)
  | y - x >= 2 * n = this : createCells (x + 1) (- 2 * n - x - 1)
  | otherwise = this : createCells x (y + 2)
  where
    this = Cell { xx = x, yy = y, insects = []}
    n = numberOfPieces


-- | Создаем список из клеток, в которых вначале находятся фишки
createPieces :: [Picture] -> Board
createPieces pic =
  [ Cell{xx = -x, yy = -10, insects = [(Beige, Queen, t 0)]}
  , Cell{xx = -x, yy = -8, insects = [(Beige, Spider, t 1)]}
  , Cell{xx = -x, yy = -6, insects = [(Beige, Spider, t 1)]}
  , Cell{xx = -x, yy = -4, insects = [(Beige, Beetle, t 2)]}
  , Cell{xx = -x, yy = -2, insects = [(Beige, Beetle, t 2)]}
  , Cell{xx = -x, yy = 0, insects = [(Beige, Hopper, t 3)]}
  , Cell{xx = -x, yy = 2, insects = [(Beige, Hopper, t 3)]}
  , Cell{xx = -x, yy = 4, insects = [(Beige, Hopper, t 3)]}
  , Cell{xx = -x, yy = 6, insects = [(Beige, Ant, t 4)]}
  , Cell{xx = -x, yy = 8, insects = [(Beige, Ant, t 4)]}
  , Cell{xx = -x, yy = 10, insects = [(Beige, Ant, t 4)]}

  , Cell{xx = x, yy = -10, insects = [(Black, Queen, t 5)]}
  , Cell{xx = x, yy = -8, insects = [(Black, Spider, t 6)]}
  , Cell{xx = x, yy = -6, insects = [(Black, Spider, t 6)]}
  , Cell{xx = x, yy = -4, insects = [(Black, Beetle, t 7)]}
  , Cell{xx = x, yy = -2, insects = [(Black, Beetle, t 7)]}
  , Cell{xx = x, yy = 0, insects = [(Black, Hopper, t 8)]}
  , Cell{xx = x, yy = 2, insects = [(Black, Hopper, t 8)]}
  , Cell{xx = x, yy = 4, insects = [(Black, Hopper, t 8)]}
  , Cell{xx = x, yy = 6, insects = [(Black, Ant, t 9)]}
  , Cell{xx = x, yy = 8, insects = [(Black, Ant, t 9)]}
  , Cell{xx = x, yy = 10, insects = [(Black, Ant, t 9)]}]
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
drawAllCells board = scale cx cy (pictures (map drawCell board))
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY


-- | Рисуем клетку
drawCell :: Cell -> Picture
drawCell (Cell {xx = x, yy = y}) = color  black (line
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
drawAllInsects board = pictures(map drawInsect board)


-- | Рисуем самое верхнее насекомое в клетке
drawInsect :: Cell -> Picture
drawInsect (Cell {insects = []}) = blank
drawInsect (Cell {xx = x, yy = y, insects = (_,_,pic):_}) =
  translate kx ky pic
  where
    kx = fromIntegral (cellSizeX * x)
    ky = fromIntegral (cellSizeY * y)


-- =========================================
-- Обработка событий
-- =========================================


-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) = placeMark (mouseToCell mouse)
handleGame _ = id
-- handleGame (EventKey (MouseButton RightButton) Down _ mouse) = placeMark (mouseToCell mouse)

-- | Обновление игры.
updateGame :: Float -> Game -> Game
updateGame _ = id


-- | Передвижение фишек (пока что в любую позицию любым игроком)
placeMark :: (Int, Int) -> Game -> Game
placeMark (i, j) game =
  case winner (gameBoard game) of
      Just _ -> game    -- | если есть победитель/ничья, то поставить фишку нельзя 
      Nothing ->
        case movablePieceInGame game of -- как уже говорили - если хранится Nothing, то новый ход, если фишка - то ставим ее
            Nothing | maybeCellToCell selectedCell == Cell  { xx = i, -- тыкнули по пустому полю или клетке => фишка не выбрана для движения
                                                              yy = j, 
                                                              insects = []
                                                              } -> game 
                    |otherwise -> game -- | клетка с насекомым - удаляем с поля для дальнейшего передвижения
                        { gameBoard  = newBoardDel
                        , gamePlayer = switchPlayer (gamePlayer game)
                        , movablePieceInGame = (pieceDel (insects (maybeCellToCell selectedCell))) -- Запоминаем удаленную фишку
                        }
            movePiece | maybeCellToCell selectedCell == Cell { xx = i -- какая-то клетка движется и выбранное поле пусто - поставить фишку
                                                             , yy = j 
                                                             , insects = []
                                                             } -> game { gameBoard  = putInsect Cell{xx = xx (maybeCellToCell selectedCell) , yy = yy (maybeCellToCell selectedCell), insects = maybePieceToListPiece movePiece} (gameBoard game)
-- не смогла запихнуть <putInsect...> в where - так как тогда не виден аргумент movePiece, инет пишет что надо юзать let - но я видно криворукая, let не вышел))) )
                                                                       , gamePlayer = switchPlayer (gamePlayer game)
                                                                       , movablePieceInGame = Nothing
                                                                       }
                      | otherwise -> game
  where 
    selectedCell = find (\cell -> ((xx cell == i) && (yy cell == j))) (gameBoard game)
    newBoardDel = deleteInsect (maybeCellToCell selectedCell) (gameBoard game)
    pieceDel :: [Piece] -> Maybe Piece -- Удаляемая фишку - из хвоста списка - то есть самую верхнюю на конкретной клетке
    pieceDel [] = Nothing
    pieceDel [x] = Just x
    pieceDel (_:xs) = pieceDel xs
  
-- | Переводит Maybe Cell в Cell
maybeCellToCell :: Maybe Cell -> Cell
maybeCellToCell(Just p) = p
maybeCellToCell Nothing = Cell{xx = 0, yy = 0, insects = []} -- просто пустая клетка

-- | Переводит Maybe Piece в [Piece]
maybePieceToListPiece :: Maybe Piece -> [Piece]
maybePieceToListPiece(Just p) = [p]
maybePieceToListPiece Nothing = []
  
-- | Удаление фишки из старой позиции (перед перемещением)
deleteInsect :: Cell -> Board -> Board
deleteInsect _ [] = []
deleteInsect oldCell (x:xs) | oldCell == x = (Cell { xx = xx oldCell, yy = yy oldCell, insects = []}) : xs
                            | otherwise = x : deleteInsect oldCell xs

-- | Постановка фишки в новую позицию (после перемещения)
putInsect :: Cell -> Board -> Board
putInsect cell [] = [cell]
putInsect cell (x:xs) | cell == x = (Cell { xx = xx cell, yy = yy cell, insects = insects cell}) : xs
                      | otherwise = x : putInsect cell xs

-- | Сменить текущего игрока
switchPlayer :: Player -> Player
switchPlayer Beige = Black
switchPlayer Black = Beige

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = round (x / fromIntegral cellSizeX)
    j = round (y / fromIntegral cellSizeY)


winner :: Board -> Maybe GameEnding
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
