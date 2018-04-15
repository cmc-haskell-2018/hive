module Hive.Render.Draw
  where

import Hive.Model
import Hive.Config
import Hive.Control
import Hive.Render.Almost
import qualified Data.Map as Map
import Graphics.Gloss


-- =========================================
-- Отрисовка игры
-- =========================================


-- | Рисуем всё
drawGame :: Game -> Picture
drawGame game@Game{gameBoard = board, gameEnding = maybeEnding, gameMovable = movable
            , gamePlayer = player} = pictures
  [ drawAllInsects board
  , drawEnding maybeEnding
  , drawMovable movable
  , drawMove maybeEnding player
  , drawInsBeetle board  -- рисует окружности на верхней фишке, кол-во окружностей кол-во жуков в стопке фишек. (максимум 5 = 4 жука + любое насекомое) 
  , drawDemand game
  , drawPossibleMoves game
  , drawAboutToLose game
  ]

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
drawDemand :: Game -> Picture
drawDemand Game{gameEnding = maybeEnding, gameMovable = movable
            , gamePlayer = player, gameStepBeige = stepBeige, gameStepBlack = stepBlack}
  | player == Beige && stepBeige == Fours && maybeEnding == Nothing && movable == Nothing = writeDemand
  | player == Black && stepBlack == Fours && maybeEnding == Nothing && movable == Nothing = writeDemand
  | otherwise = blank

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

-- | Надпись, если шах
drawAboutToLose :: Game -> Picture
drawAboutToLose game
  | aboutToLose game = placeText $ text "You are about to lose"
  | otherwise = blank
  where
    placeText = (translate (fromIntegral screenWidth / 2 - 460) (fromIntegral screenHeight / 2 - 60)) .
        scale 0.3 0.3


