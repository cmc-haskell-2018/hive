{-# OPTIONS_GHC -Wall #-}
module Hive where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.List
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
  deriving (Show, Eq, Enum)
  
-- | Координаты клетки
type Coord = (Int, Int)

-- | Фишка
type Piece = (Player, Insect, Picture)

-- | Клетка может содержать несколько фишек
type Cell = [Piece]

-- | Поле
type Board = Map.Map Coord Cell

-- | Достижимые клетки (для проверки улья на разрывность)
type AccCells = Map.Map Coord Cell

-- | Фишка с координатами
type Movable = (Coord, Piece)

-- | Игрок
data Player = Beige | Black
  deriving (Show, Eq, Ord)

-- | Окончание игры
data Ending = Win Player | Tie

-- | Контроль количества шагов
data Step = First | Second | Third | Fours | Other 
  deriving (Enum, Eq)

-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , gameMovable :: Maybe Movable  -- Nothing - никакая фишка не перемещается, иначе - указана перемещаемая фишка.
  , gameEnding :: Maybe Ending    -- Nothing - игра не окончена.
  , gameStepBlack :: Step -- Номер хода черного игрока 
  , gameStepBeige :: Step -- Номер хода бежевого игрока 
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
  , gameStepBlack = First -- первый ход черного
  , gameStepBeige = First -- первый ход бежевого
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
    k = 5 / 4 * fromIntegral cellSizeX / (fromIntegral pieceWidth)

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
drawGame game@Game{gameBoard = board, gameEnding = maybeEnding, gameMovable = movable
            , gamePlayer = player, gameStepBeige = stepBeige, gameStepBlack = stepBlack} = pictures
  [ --drawAllCells board,
    drawAllInsects board
  , drawEnding maybeEnding
  , drawMovable movable
  , drawMove maybeEnding player
  , drawDemand stepBeige stepBlack player maybeEnding movable
  , drawPossibleMoves game]

-- | Проверяем, нужно ли рисовать возможные ходы
drawPossibleMoves :: Game -> Picture
drawPossibleMoves Game{gameMovable = Nothing} = blank
drawPossibleMoves game = drawPossible $ possibleMoves game

-- | Рисуем возможные ходы
drawPossible :: [Coord] -> Picture
drawPossible coords = color (greyN 0.5) $ scale cx cy $ pictures $ map drawCell coords
  where
  cx = fromIntegral cellSizeX
  cy = fromIntegral cellSizeY

scaleText :: Picture -> Picture
scaleText = scale scaleFactor scaleFactor
  where
    scaleFactor = (fromIntegral cellSizeX)/105

-- | Рисуем передвигаемую фишку и соответствующий текст
drawMovable :: Maybe Movable -> Picture
drawMovable Nothing = blank
drawMovable (Just (_, (Beige, _, pic))) = pictures
  [
    translate
      (halfScreenWidth  + 50)
      (halfScreenHeight + 50)
    $ scale 2 2 pic
  , translate
      (halfScreenWidth  + 20)
      (halfScreenHeight + 100)
    $ scaleText $ text "You are holding"
  ]
  where
    halfScreenWidth  = fromIntegral (-screenWidth)  / 2
    halfScreenHeight = fromIntegral (-screenHeight) / 2
    

drawMovable (Just (_, (Black, _, pic))) = pictures
  [
    translate
      (halfScreenWidth  - 50)
      (halfScreenHeight + 50)
    $ scale 2 2 pic
  , translate
      (halfScreenWidth  - 320)
      (halfScreenHeight + 100)
    $ scaleText $ text "You are holding"]
  where
    halfScreenWidth  = fromIntegral   screenWidth   / 2
    halfScreenHeight = fromIntegral (-screenHeight) / 2

-- | Пишем, чей ход
drawMove :: Maybe Ending -> Player -> Picture
drawMove (Just _) _ = blank
drawMove Nothing player = placeText $ text $ (show player) ++ " team's move"
  where
    placeText =
        (translate
          (fromIntegral (-screenWidth) / 2 + 20)
          (fromIntegral   screenHeight / 2 - 60)
        ) . scaleText

-- | Проверяем, нужно ли взять пчелу
drawDemand :: Step -> Step -> Player -> Maybe Ending -> Maybe Movable -> Picture
drawDemand Fours _ Beige Nothing Nothing = writeDemand
drawDemand _ Fours Black Nothing Nothing = writeDemand
drawDemand _ _ _ _ _ = blank

-- | Пишем, что нужно взять пчелу
writeDemand :: Picture
writeDemand = placeText $ text "Take the Queen bee"
  where
    placeText =
        (translate
          (fromIntegral screenWidth  / 2 - 420)
          (fromIntegral screenHeight / 2 - 60)
        ) . scaleText

-- | Рисуем все клетки
drawAllCells :: Board -> Picture
drawAllCells board = color  (greyN 0.3) $ scale cx cy $ pictures $ map drawCell tl
  where
    cx = fromIntegral cellSizeX
    cy = fromIntegral cellSizeY
    tl = map fst $ Map.toList board

-- | Рисуем клетку
drawCell :: Coord -> Picture
drawCell (x, y) = line
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
    placeText =
      (translate
        (fromIntegral (- screenWidth) / 2 + 20)
        (fromIntegral screenHeight / 2 - 60)
      ) . scaleText

-- | Надпись об окончании игры
endingText :: Ending -> String
endingText Tie = "It's a Tie:)"
endingText (Win Black) = "Black Team Won"
endingText (Win Beige) = "Beige Team Won"


-- =========================================
-- Перемещение фишек
-- =========================================


-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = takePiece mouse game    -- фишка еще не взята
  | otherwise = checkWinner $ shiftGame $ 
        makeMove (mouseToCell mouse (gameBoard game)) game    -- фишка уже взята
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
getCell (xx, yy) -- = if ((i+j) mod 2) == 0 then (i,j) else
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
  | pieces == [] = game
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
deleteInsect (i, j) board
  | pieces == [] = Map.delete (i, j) board
  | otherwise = Map.adjust tail (i, j) board
  where
    pieces = fromMaybe [] $ Map.lookup (i, j) board    -- список фишек в клетке с нужными координатами

-- | Получить клетку под мышкой (в которую хотим поставить фишку).
mouseToCell :: Point -> Board -> Maybe Coord
mouseToCell (x, y) board
  | Map.lookup coord board == Nothing = Nothing
  | otherwise = Just coord
  where
    coord = getCell (x, y)

-- | Сделать ход, если возможно
makeMove :: Maybe Coord -> Game -> Game
makeMove Nothing game = game    -- если ткнули не в клетку поля
makeMove (Just (i, j)) game@Game{gamePlayer = player, gameBoard = board, gameMovable = Just movable, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
   | (elem (i, j) (possibleMoves game)) = Game    -- если выбранный ход возможен
     { gamePlayer = switchPlayer player
     , gameBoard = putInsect (snd movable) (i,j) board
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
putInsect piece = Map.adjust (piece:)


-- =========================================
-- Общие функции для определения возможных ходов
-- =========================================


-- | Список координат всех допустимых клеток для постановки фишки
possibleMoves :: Game -> [Coord]
possibleMoves Game{gameBoard = board, gameMovable = Just((x,y), (player, ins,_)), gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  | isSide && step == First = notTearingMoves board $ delStartCells (map fst $ Map.toList only_free_cells)
  | isSide = notTouchingPieces (switchPlayer player) board $ notTearingMoves board $ delStartCells (map fst $ Map.toList only_free_cells)
  | step /= Other = []
  | ins == Queen  = notTearingMoves board $ delStartCells (queen_cells (x,y) board) 
  | ins == Beetle = notTearingMoves board $ beetle_cells (x,y) (delStartCells (map fst $ Map.toList board))
  | ins == Hopper = notTearingMoves board $ hopper_cells (x,y) board
  | ins == Ant = antMoves (x, y) board
  | otherwise = spiderMoves (x, y) board
 where
  step = if player == Beige then stepBeige else stepBlack
  isSide = x < -(n+1) || x > n+1
  n = numberOfPieces
  only_free_cells = Map.filter (\val -> val == []) board
possibleMoves _ = []

-- | удаляет из списка координат стартовые клетки
delStartCells :: [Coord] -> [Coord]
delStartCells [] = []
delStartCells l = filter (\(x,_) -> x >= -(n+1) && x <= n+1 ) l
 where n = numberOfPieces

-- | координаты для королевы и жука
-- |Пчеломатка может перемещаться всего на 1 "клетку". Жук, также как и пчеломатка, может перемещаться только на 1 позицию за
-- |ход. Но в отличии от всех остальных фишек, он может перемещать поверх других фишек.

beetle_cells :: Coord -> [Coord] -> [Coord]
beetle_cells (x,y) = filter (\(a,b) -> (a,b) == (x-1, y+1) 
  ||  (a,b) == (x+1,y+1)
  ||  (a,b) == (x,y+2)
  ||  (a,b) == (x,y-2)
  ||  (a,b) == (x-1,y-1)
  ||  (a,b) == (x+1,y-1)
   )

queen_cells :: Coord -> Board -> [Coord]
queen_cells (x,y) board 
  | board == Map.empty = []
  | otherwise = map fst $ Map.toList only_free_cells
 where 
    l = coord1 ++ coord2 ++ coord3 ++ coord4 ++ coord5 ++ coord6
    only_free_cells = Map.filter (\val -> val == []) (keysToBoard l board )
    coord1 = if Map.lookup (x, y+2)   board ==  (Just []) && Map.lookup (x+1, y+1)board /= (Just []) && Map.lookup (x-1, y+1)board/= (Just []) then [] else [(x,y+2)] 
    coord2 = if Map.lookup (x+1, y+1) board ==  (Just []) && Map.lookup (x, y+2)  board /= (Just []) && Map.lookup (x+1, y-1)board/= (Just []) then [] else [(x+1,y+1)]
    coord3 = if Map.lookup (x+1, y-1) board ==  (Just []) && Map.lookup (x+1, y+1)board /= (Just []) && Map.lookup (x, y-2)  board/= (Just []) then [] else [(x+1,y-1)]
    coord4 = if Map.lookup (x, y-2)   board ==  (Just []) && Map.lookup (x+1, y-1)board /= (Just []) && Map.lookup (x-1, y-1)board/= (Just []) then [] else [(x,y-2)]
    coord5 = if Map.lookup (x-1, y-1) board ==  (Just []) && Map.lookup (x, y-2)  board /= (Just []) && Map.lookup (x-1, y+1)board/= (Just []) then [] else [(x-1,y-1)] 
    coord6 = if Map.lookup (x-1, y+1) board ==  (Just []) && Map.lookup (x-1, y-1)board /= (Just []) && Map.lookup (x, y+2)  board/= (Just []) then [] else [(x-1,y+1)]
    
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


-- | Только ходы, не разрывающие улья
notTearingMoves :: Board -> [Coord] -> [Coord]
notTearingMoves board = filter (\coord -> doesNotTear coord board)

-- | Проверить, что ход не разрывает улья
doesNotTear :: Coord -> Board -> Bool
doesNotTear coord board = accSize + sideSize == 2 * n - 1
  where
    accSize = Map.foldr (\x y -> length x + y) 0 (accessibleCells coord board)    --  количество фишек на поле, достижимых из данной клетки
    sideSize = Map.size (Map.filterWithKey (\(x, _) ins -> (x < -(n+1) || x > n+1) && ins /= []) board)     -- количество не введенных в игру фишек
    n = numberOfPieces

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
  | isSide = accCells
  | check == [] = accCells
  | Map.member coord accCells = accCells
  | otherwise = 
    checkForPieces (x-1, y+1) board $
    checkForPieces (x, y+2) board $
    checkForPieces (x+1, y+1) board $
    checkForPieces (x+1, y-1) board $
    checkForPieces (x, y-2) board $
    checkForPieces (x-1, y-1) board newAccCells
      where
        newAccCells = Map.insert coord check accCells     -- вставить текущую клетку в составляемый список
        check = fromMaybe [] (Map.lookup coord board)     -- возвращает список фишек в клетке, а также пустой список, если такой клетки нет 
        isSide      -- находится ли фишка на границе или за границей игрового поля
          | y - x >= 2 * (n+1) = True
          | x <= -(n+1) = True
          | x + y <= -2 * (n+1) = True
          | x - y >= 2 * (n+1) = True
          | x >= n+1 = True
          | x + y >= 2 * (n+1) = True
          | otherwise = False
            where n = numberOfPieces


-- =========================================
-- Все, что относится к пауку
-- =========================================

-- | Возможные ходы для паука
spiderMoves :: Coord -> Board -> [Coord]
spiderMoves coord board = nub $ spiderSteps 0 coord board [coord] 

-- | Рекурсивно собираем возможные ходы паука
spiderSteps :: Int -> Coord -> Board -> [Coord] -> [Coord]
spiderSteps 3 coord _ _ = [coord]
spiderSteps n (x, y) board passed = collectUp ++ collectUpLeft ++ collectUpRight ++
                                    collectDown ++ collectDownLeft ++ collectDownRight
  where
    isEmpty c = fromMaybe [(Black, Queen, blank)] c == []    -- пустая ли клетка
    isFull c = fromMaybe [] c /= []    -- заполнена ли клетка
    up = Map.lookup (x, y+2) board    -- верхняя клетка
    upLeft = Map.lookup (x-1, y+1) board    -- верхняя левая клетка
    upRight = Map.lookup (x+1, y+1) board    -- верхняя правая клетка
    down = Map.lookup (x, y-2) board    -- нижняя клетка
    downLeft = Map.lookup (x-1, y-1) board    -- нижняя левая клетка
    downRight = Map.lookup (x+1, y-1) board    -- нижняя правая клетка
    
    collectUp = if isEmpty up && (isEmpty upLeft && isFull upRight || isFull upLeft && isEmpty upRight)
                        && doesNotTear (x, y+2) board && not (elem (x, y+2) passed)       -- собрать ходы сверху
                    then spiderSteps (n+1) (x, y+2) board ((x, y+2):passed) else []
    collectUpLeft = if isEmpty upLeft && (isEmpty up && isFull downLeft || isEmpty downLeft && isFull up)
                        && doesNotTear (x-1, y+1) board && not (elem (x-1, y+1) passed)       -- собрать ходы сверху слева
                    then spiderSteps (n+1) (x-1, y+1) board ((x-1, y+1):passed) else []
    collectUpRight = if isEmpty upRight && (isEmpty up && isFull downRight || isEmpty downRight && isFull up) 
                        && doesNotTear (x+1, y+1) board && not (elem (x+1, y+1) passed)       -- собрать ходы сверху справа
                    then spiderSteps (n+1) (x+1, y+1) board ((x+1, y+1):passed) else []
    collectDown = if isEmpty down && (isEmpty downLeft && isFull downRight || isEmpty downRight && isFull downLeft)
                        && doesNotTear (x, y-2) board && not (elem (x, y-2) passed)       -- собрать ходы снизу
                    then spiderSteps (n+1) (x, y-2) board ((x, y-2):passed) else []
    collectDownLeft = if isEmpty downLeft && (isEmpty upLeft  && isFull down || isEmpty down && isFull upLeft)
                        && doesNotTear (x-1, y-1) board && not (elem (x-1, y-1) passed)       -- собрать ходы снизу слева
                    then spiderSteps (n+1) (x-1, y-1) board ((x-1, y-1):passed) else []
    collectDownRight = if isEmpty downRight && (isEmpty down  && isFull upRight || isEmpty upRight && isFull down)
                        && doesNotTear (x+1, y-1) board && not (elem (x+1, y-1) passed)       -- собрать ходы снизу справа
                    then spiderSteps (n+1) (x+1, y-1) board ((x+1, y-1):passed) else []


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
    up = Map.lookup (x, y+2) board == Just []    -- пустая ли верхняя клетка
    upLeft = Map.lookup (x-1, y+1) board == Just []    -- пустая ли верхняя левая клетка
    upRight = Map.lookup (x+1, y+1) board == Just []    -- пустая ли верхняя правая клетка
    down = Map.lookup (x, y-2) board == Just []    -- пустая ли нижняя клетка
    downLeft = Map.lookup (x-1, y-1) board == Just []    -- пустая ли нижняя левая клетка
    downRight = Map.lookup (x+1, y-1) board == Just []    -- пустая ли нижняя правая клетка
    
    collectUp acc = if up && (upLeft || upRight)
                        && doesNotTear (x, y+2) board && not (elem (x, y+2) acc)       -- собрать ходы сверху
                    then collectAntMoves (x, y+2) board ((x, y+2):acc) else acc
    collectUpLeft acc = if upLeft && (up || downLeft)
                        && doesNotTear (x-1, y+1) board && not (elem (x-1, y+1) acc)       -- собрать ходы сверху слева
                    then collectAntMoves (x-1, y+1) board ((x-1, y+1):acc) else acc
    collectUpRight acc = if upRight && (up || downRight)
                        && doesNotTear (x+1, y+1) board && not (elem (x+1, y+1) acc)       -- собрать ходы сверху справа
                    then collectAntMoves (x+1, y+1) board ((x+1, y+1):acc) else acc
    collectDown acc = if down && (downLeft || downRight)
                        && doesNotTear (x, y-2) board && not (elem (x, y-2) acc)       -- собрать ходы снизу
                    then collectAntMoves (x, y-2) board ((x, y-2):acc) else acc
    collectDownLeft acc = if downLeft && (upLeft || down)
                        && doesNotTear (x-1, y-1) board && not (elem (x-1, y-1) acc)       -- собрать ходы снизу слева
                    then collectAntMoves (x-1, y-1) board ((x-1, y-1):acc) else acc
    collectDownRight acc = if downRight && (down || upRight)
                        && doesNotTear (x+1, y-1) board && not (elem (x+1, y-1) acc)       -- собрать ходы снизу справа
                    then collectAntMoves (x+1, y-1) board ((x+1, y-1):acc) else acc


-- =========================================
-- Все, что относится к кузнечику
-- =========================================


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
  -- ищем в каждом направлении первую непустую клетку
  -- фльтром получаем из данного списка клеток  - l координаты клеток в одном из 6 направлений, 
  -- dir1, dir3, dir4 идут по порядку следования, dir2 dir5 dir6 наоборот, поэтому делаем reverse 
  | otherwise =  delOneCoord (x,y+2) (takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir1 )l) board) False) ++
                 delOneCoord (x,y-2) (takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir2 )l) board) True)  ++
                 delOneCoord (x+1,y+1)(takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir3 )l) board) False) ++ 
                 delOneCoord (x+1,y-1)(takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir4 )l) board) False) ++
                 delOneCoord (x-1,y+1)(takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir5 )l) board) True) ++
                 delOneCoord (x-1,y-1)(takeCell (keysToBoard (filter (\(a,b) -> elem (a,b) dir6 )l) board) True) 
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
takeCell :: Board -> Bool->[Coord] 
takeCell board flag  
  | l == [] = []
  | snd head_l == [] =  [fst head_l] 
  | otherwise =  takeCell tail_map flag
  where
    l = if flag then reverse (Map.toList board) else (Map.toList board) 
    head_l = head l 
    tail_map = Map.fromList (tail l)

-- выдает по номеру список клеток в которым прыгает кузнечик, всего 6 направлений, я их отдельно обрабатываю, собственно из-за этого  и существует  for_hopper
for_hopper :: Int-> Coord -> [Coord] -> [Coord]
for_hopper n (x,y) [(max_x,max_y),(min_x,min_y)]
 | n > 6 || n < 1 = [] 
 |n == 1 =  zip [x,x ..] [y+2, y+4 .. max_y] --список координат y через 2 позиции y > 0
 |n == 2 =  zip [x,x ..] [y-2, y-4 .. min_y] --список координат y через 2 позиции y < 0
 |n == 3 =  zip [x+1,x+2 .. max_x] [y+1,y+2 .. max_y]
 |n == 4 =  zip [x+1,x+2 .. max_x] [y-1,y-2 .. min_y]
 |n == 5 =  zip [x-1,x-2 .. min_x] [y+1,y+2 .. max_y]
 |n == 6 =  zip [x-1,x-2 .. min_x] [y-1,y-2 .. min_y]
 |otherwise = []
for_hopper _ _ _ = []


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
-- Все, что относится к определению победителя
-- =========================================


-- | Установить gameEnding в Game, если игра завершилась
checkWinner :: Game -> Game
checkWinner game = game{gameEnding = winner (gameBoard game)}

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


-- =========================================
-- Все, что относится к сдвигу массива фишек
-- =========================================


-- | Это просто для вызова shiftBoard,
-- потому что делать shiftBoard еще больше я замучаюсь
shiftGame  :: Game -> Game
shiftGame game@Game{gameBoard = board} = game{gameBoard = shiftBoard board}

-- | Передвинуть массив фишек, если он касается края поля
shiftBoard :: Board -> Board
shiftBoard board
  | Map.filterWithKey (\(x, _) a -> x == -(n+1) && a/=[]) board /= Map.empty    --  если коснулись левой границы
      = shiftBoard $ Map.unions
        [left, leftDown, withoutUpRight $ shiftCoord (1, 1) board]
  | Map.filterWithKey (\(x, y) a -> x >= -(n+1) && y-x == 2*(n+1) && a/=[]) board /= Map.empty    --  если коснулись левой верхней границы
      = shiftBoard $ Map.unions
        [left, leftUp, withoutDownRight $ shiftCoord (1, -1) board]
  | Map.filterWithKey (\(x, y) a -> x <= n+1 && x+y == 2*(n+1) && a/=[]) board /= Map.empty    --  если коснулись правой верхней границы
      = shiftBoard $
        Map.unions [leftUp, rightUp, withoutDown $ shiftCoord (0, -2) board]
  | Map.filterWithKey (\(x, _) a -> x == n+1 && a/=[]) board /= Map.empty    --  если коснулись правой границы
      = shiftBoard $
        Map.unions [rightUp, right, withoutDownLeft $ shiftCoord (-1, -1) board]
  | Map.filterWithKey (\(x, y) a -> x <= n+1 && x-y == 2*(n+1) && a/=[]) board /= Map.empty    --  если коснулись правой нижней границы
      = shiftBoard $
        Map.unions [right, rightDown,withoutUpLeft $ shiftCoord (-1, 1) board]
  | Map.filterWithKey (\(x, y) a -> x >= -(n+1) && x+y == -2*(n+1) && a/=[]) board /= Map.empty    --  если коснулись левой нижней границы
      = shiftBoard $
        Map.unions [rightDown, leftDown, withoutUp $ shiftCoord (0, 2) board]
  | otherwise = board
    where
      left = Map.fromList $ map (\y -> ((-(n+1), y), [])) [-(n+1),-(n-1)..n+1]   -- левая граница поля
      leftUp = Map.fromList $ map (\x -> ((x, 2*(n+1)+x), [])) [-(n+1)..0]     -- левая верхняя граница поля
      leftDown = Map.fromList $ map (\x -> ((x, -2*(n+1)-x), [])) [-(n+1)..0]     -- левая нижняя граница поля
      rightUp = Map.fromList $ map (\x -> ((x, 2*(n+1)-x), [])) [0..n+1]    -- правая верхняя граница поля
      right = Map.fromList $ map (\y -> ((n+1, y), [])) [n+1,n-1.. -(n+1)]    -- правая граница поля
      rightDown = Map.fromList $ map (\x -> ((x, x-2*(n+1)), [])) [0..n+1]       -- правая нижняя граница поля
      withoutUpRight = Map.filterWithKey (\(x, y) _ -> x+y /= 2*(n+2) && x /= n+2 || isSide x)      -- без лишних клеток сверху справа
      withoutDownRight = Map.filterWithKey (\(x, y) _ -> x-y /= 2*(n+2) && x /= n+2 || isSide x)      -- без лишних клеток снизу справа
      withoutDown = Map.filterWithKey (\(x, y) _ -> x+y /= -2*(n+2) && x-y /= 2*(n+2) || isSide x)      -- без лишних клеток снизу
      withoutDownLeft = Map.filterWithKey (\(x, y) _ -> x+y /= -2*(n+2) && x /= -(n+2) || isSide x)      -- без лишних клеток снизу слева
      withoutUpLeft = Map.filterWithKey (\(x, y) _ -> y-x /= 2*(n+2) && x /= -(n+2) || isSide x)      -- без лишних клеток сверху слева
      withoutUp = Map.filterWithKey (\(x, y) _ -> y-x /= 2*(n+2) && x+y /= 2*(n+2) || isSide x)      -- без лишних клеток сверху
      n = numberOfPieces
      isSide x = x > n+2 || x < -(n+2)
      shiftCoord :: (Int, Int) -> Board -> Board    -- сдвигает поле на (i, j)
      shiftCoord (i, j) = Map.mapKeys (\(x, y) -> if (x>= -(n+1)) && (x<= n+1)
                                                    then(x+i, y+j) else (x, y))

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
  (show <$> [Queen .. Ant])