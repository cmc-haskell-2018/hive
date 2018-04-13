{-# OPTIONS_GHC -Wall #-}
module Hive where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Data.List
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
  deriving (Show, Eq, Enum)
  
-- | Координаты клетки
type Coord = (Int, Int)

-- | Фишка
type Piece = (Player, Insect, Picture)

-- | Клетка может содержать несколько фишек
type Cell = [Piece]

-- | Поле
type Board = Map Coord Cell

-- | Достижимые клетки (для проверки улья на разрывность)
type AccCells = Map Coord Cell

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
  { gameBoard  = createPieces images    -- клеточки по краям
  , gamePlayer = Beige    -- первый игрок ходит бежевыми
  , gameMovable = Nothing    -- фишка пока что не перемещается
  , gameEnding = Nothing    -- игра не окончена
  , gameStepBlack = First -- первый ход черного
  , gameStepBeige = First -- первый ход бежевого
  }

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
  [ drawAllInsects board
  , drawEnding maybeEnding
  , drawMovable movable
  , drawMove maybeEnding player
  , drawInsBeetle board  -- рисует окружности на верхней фишке, кол-во окружностей кол-во жуков в стопке фишек. (максимум 5 = 4 жука + любое насекомое) 
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

-- | Рисует стопки жуков
drawInsBeetle ::  Board -> Picture
drawInsBeetle board 
  | board == Map.empty = blank 
  | moreInsCells board == Map.empty = blank
  | otherwise = drawCircle (moreInsCells board)

-- | Возвращает координаты клетки на которых стоят несколько насекомых
moreInsCells :: Board -> Board  
moreInsCells board 
  | board == Map.empty = Map.empty
  | otherwise = Map.filter (\val -> length val > 1) board

-- | Рисовать окружности если фишки стоят друг на друге 
drawCircle :: Board -> Picture 
drawCircle board = pictures(map drawOneStack tl)
  where
    tl = Map.toList board

-- | Рисует на одной стопке 
drawOneStack :: (Coord,Cell)-> Picture
drawOneStack (_, []) = blank
drawOneStack ((x, y), l) =
  pictures [translate kx ky $ scale 0.15 0.15 $ color red $ drawCir n] 
  where
    kx = fromIntegral (cellSizeX * x)
    ky = fromIntegral (cellSizeY * y)
    n = length l

-- | Рекурсивоно рисует столько окружностей сколько насекомых в стопке
drawCir :: Int -> Picture
drawCir n 
  | n == 1 = pictures [Circle r]
  |n > 1 =  pictures [Circle r , drawCir (n-1)] 
  | otherwise = blank
  where
    r = fromIntegral (12 * 2 * n) -- 12 и 2, потому что это самые подходящие константы для того, чтобы были видны окружности.

-- | Рисуем передвигаемую фишку и соответствующий текст
drawMovable :: Maybe Movable -> Picture
drawMovable Nothing = blank
drawMovable (Just (_, (Beige, _, pic))) = pictures
  [ translate (fromIntegral (- screenWidth) / 2 + 50) (fromIntegral (-screenHeight) / 2 + 50) $
        scale 2 2 pic
  , translate (fromIntegral (- screenWidth) / 2 + 20) (fromIntegral (-screenHeight) / 2 + 100) $
        scale 0.3 0.3 $ text "You are holding"]
drawMovable (Just (_, (Black, _, pic))) = pictures
  [ translate (fromIntegral screenWidth / 2 - 50) (fromIntegral (-screenHeight) / 2 + 50) $
        scale 2 2 pic
  , translate (fromIntegral screenWidth / 2 - 320) (fromIntegral (-screenHeight) / 2 + 100) $
        scale 0.3 0.3 $ text "You are holding"]

-- | Пишем, чей ход
drawMove :: Maybe Ending -> Player -> Picture
drawMove (Just _) _ = blank
drawMove Nothing player = placeText $ text $ (show player) ++ " team's move"
  where
    placeText = (translate (fromIntegral (- screenWidth) / 2 + 20) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.3 0.3

-- | Проверяем, нужно ли взять пчелу
drawDemand :: Step -> Step -> Player -> Maybe Ending -> Maybe Movable -> Picture
drawDemand Fours _ Beige Nothing Nothing = writeDemand
drawDemand _ Fours Black Nothing Nothing = writeDemand
drawDemand _ _ _ _ _ = blank

-- | Пишем, что нужно взять пчелу
writeDemand :: Picture
writeDemand = placeText $ text "Take the Queen bee"
  where
    placeText = (translate (fromIntegral screenWidth / 2 - 420) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.3 0.3

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
    placeText = (translate (fromIntegral (- screenWidth) / 2 + 20) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.3 0.3

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
        makeMove (getCell mouse) game    -- фишка уже взята
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
getCell (xx, yy)
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
  | pieces == [] = game     -- фишки с такими координатами нет
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
deleteInsect coord board
  | tail pieces == [] = Map.delete coord board
  | otherwise = Map.adjust tail coord board
  where
    pieces = fromMaybe [] $ Map.lookup coord board    -- список фишек в клетке с нужными координатами


-- | Сделать ход, если возможно
makeMove :: Coord -> Game -> Game
makeMove coord game@Game{gamePlayer = player, gameBoard = board, gameMovable = Just movable, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
   | (elem coord (possibleMoves game)) = Game    -- если выбранный ход возможен
     { gamePlayer = switchPlayer player
     , gameBoard = putInsect (snd movable) coord board
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
putInsect piece coord board
  | Map.member coord board = Map.adjust (piece:) coord board
  | otherwise = Map.insert coord [piece] board


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
  | otherwise = spiderMoves (x, y) board
 where
  step = if player == Beige then stepBeige else stepBlack
  isSide = x > n+1 || x < -(n+1)
  n = numberOfPieces
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


-- =========================================
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
      isNotEmpty (i, j) = Map.member (i, j) board


-- =========================================
-- Все, что относится к сдвигу массива фишек
-- =========================================


-- | Это просто для вызова shiftBoard
shiftGame  :: Game -> Game
shiftGame game@Game{gameBoard = board} = game{gameBoard = shiftBoard board}

-- | Передвинуть массив фишек, если он касается края поля
shiftBoard :: Board -> Board
shiftBoard board
  | Map.filterWithKey (\(x, _) _ -> x == -(n+1)) board /= Map.empty    --  если коснулись левой границы
      = shiftBoard $ shiftCoord (1, 1) board
  | Map.filterWithKey (\(x, y) _ -> x >= -(n+1) && y-x == 2*(n+1)) board /= Map.empty    --  если коснулись левой верхней границы
      = shiftBoard $ shiftCoord (1, -1) board
  | Map.filterWithKey (\(x, y) _ -> x <= n+1 && x+y == 2*(n+1)) board /= Map.empty    --  если коснулись правой верхней границы
      = shiftBoard $ shiftCoord (0, -2) board
  | Map.filterWithKey (\(x, _) _ -> x == n+1) board /= Map.empty    --  если коснулись правой границы
      = shiftBoard $shiftCoord (-1, -1) board
  | Map.filterWithKey (\(x, y) _ -> x <= n+1 && x-y == 2*(n+1)) board /= Map.empty    --  если коснулись правой нижней границы
      = shiftBoard $ shiftCoord (-1, 1) board
  | Map.filterWithKey (\(x, y) _ -> x >= -(n+1) && x+y == -2*(n+1)) board /= Map.empty    --  если коснулись левой нижней границы
      = shiftBoard $ shiftCoord (0, 2) board
  | otherwise = board
    where
      shiftCoord :: (Int, Int) -> Board -> Board    -- сдвигает поле на (i, j)
      shiftCoord (i, j) = Map.mapKeys (\(x, y) -> if (x>= -(n+1)) && (x<= n+1)
                                                    then(x+i, y+j) else (x, y))
      n = numberOfPieces


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


