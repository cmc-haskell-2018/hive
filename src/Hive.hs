{-# OPTIONS_GHC -Wall #-}
module Hive where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
--import Data.List
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

-- | Достижимые клетки (для проверки улья на разрывность)
type AccCells = Map Coord Cell

-- | Фишка с координатами
type Movable = (Coord, Piece)

-- | Игрок
data Player = Beige | Black
  deriving (Show, Eq, Ord)

-- | Окончание игры
data Ending = Win Player | Tie
  deriving (Eq)

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
drawGame Game{gameBoard = board, gameEnding = maybeEnding, gameMovable = movable
            , gamePlayer = player, gameStepBeige = stepBeige, gameStepBlack = stepBlack} = pictures
  [ drawAllCells board
  , drawAllInsects board
  , drawEnding maybeEnding
  , drawMovable movable
  , drawMove maybeEnding player
  , drawDemand stepBeige stepBlack player maybeEnding]

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
drawDemand :: Step -> Step -> Player -> Maybe Ending -> Picture
drawDemand Fours _ Beige Nothing = writeDemand
drawDemand _ Fours Black Nothing = writeDemand
drawDemand _ _ _ _ = blank

-- | Пишем, что нужно взять пчелу
writeDemand :: Picture
writeDemand = placeText $ text "Take the Queen bee"
  where
    placeText = (translate (fromIntegral screenWidth / 2 - 420) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.3 0.3

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
        scale 0.3 0.3

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
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | isJust (gameEnding game) = game    -- если игра окончена, ничего сделать нельзя
  | isNothing (gameMovable game) = game    -- фишка еще не взята, отменять нечего
  | otherwise = putPieceBack game       -- фишка взята, кладем ее на место
handleGame _ game = game

-- | Положить фишку на место
putPieceBack :: Game -> Game
putPieceBack game@Game{gameMovable = Just (coord, piece@(player,insect, _)), gameBoard = board, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  = game{gameMovable = Nothing, gameBoard = putInsect piece coord board, gameStepBeige = prevBeigeStep, gameStepBlack = prevBlackStep}
    where
      prevBlackStep :: Step
      prevBlackStep
        | player == Black && insect == Queen && stepBlack == Other = Fours
        | otherwise = stepBlack
      prevBeigeStep :: Step
      prevBeigeStep
        | player == Beige && insect == Queen && stepBeige == Other = Fours
        | otherwise = stepBeige    
putPieceBack game = game    -- чтобы компилятор не ругался

-- | Взять фишку с координатами под мышкой, если возможно
takePiece :: Point -> Game -> Game
takePiece (x, y) game@Game{gamePlayer = player, gameBoard = board, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  | pieces == [] = game
  | pieceColor top /= player = game
--  | possibleMoves movable (deleteInsect (i, j) board) == [] = game
  | checkQueenStep movable = newGame{ gameStepBeige = if player == Beige then Other else stepBeige
                                    , gameStepBlack = if player == Black then Other else stepBlack}
  | step == Fours = game
  | otherwise = newGame
  where
    newGame = Game
      { gameBoard = deleteInsect (i, j) board
      , gamePlayer = player
      , gameMovable = Just movable
      , gameEnding = Nothing
      , gameStepBlack = stepBlack
      , gameStepBeige = stepBeige
      }
    step = if player == Black then stepBlack else stepBeige
    i = round (x / fromIntegral cellSizeX)
    j = round (y / fromIntegral cellSizeY)
    pieces = fromMaybe [] $ Map.lookup (i, j) board    -- список фишек в клетке с нужными координатами
    top = head pieces    -- самая верхняя фишка в списке
    movable = ((i, j), top)
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
  | isNothing l = Nothing
  | otherwise = Just (i, j)
  where
    i = round (x / fromIntegral cellSizeX)
    j = round (y / fromIntegral cellSizeY)
    l = Map.lookup (i, j) board

-- | Сделать ход, если возможно
makeMove :: Maybe Coord -> Game -> Game
makeMove Nothing game = game    -- если ткнули не в клетку поля
makeMove (Just (i, j)) game@Game{gamePlayer = player, gameBoard = board, gameMovable = Just movable, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
   | (elem (i, j) (possibleMoves movable board)) = Game    -- если выбранный ход возможен
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
       nextStep x | x == Other = Other
                  | otherwise = succ x
makeMove _ game = game    -- это просто так, чтобы компилятор не ругался


-- | Поставить фишку
putInsect :: Piece -> Coord -> Board -> Board
putInsect piece = Map.adjust (piece:)

-- | Список координат всех допустимых клеток для постановки фишки (В ПРОЦЕССЕ НАПИСАНИЯ)
possibleMoves :: Movable -> Board -> [Coord]
possibleMoves ( (x,y), (_,ins,_)) board  -- flag true если мы двигаем фишку из началаьной позиции (со "старта"), иначе false, 
                                       -- в случае старта должно возвратить список всех клеток поля             
  | is_not_possible == True && ins /= Hopper && ins /= Beetle && flag == False  = []
  | flag == False && ins == Queen  = notTearingMoves board $ queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList only_free_cells)) 
  | flag == False && ins == Beetle = notTearingMoves board $ queen_beetle_cells (x,y) (delStartCells (map fst $ Map.toList board))
  | flag == False && ins == Hopper = notTearingMoves board $ hopper_cells (x,y) (delStartCells (map fst $ Map.toList only_free_cells)) 
  | otherwise = notTearingMoves board $ delStartCells (map fst $ Map.toList only_free_cells)
 where
  flag = x < -(n+1) || x > n+1
  n = numberOfPieces
  only_free_cells = Map.filter (\val -> val == []) board
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
delStartCells l = filter (\(x, _) -> x >= -(n+1) && x <= n+1 ) l
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