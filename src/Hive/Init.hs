module Hive.Init
  where

import Hive.Model
import Hive.Config
import Hive.DataBase
import Graphics.Gloss
import Graphics.Gloss.Juicy
import qualified Data.Map as Map
import Data.Foldable

-- =========================================
-- Загрузка изображений
-- =========================================


-- | Список всех имен изображений
allImageNames :: [String]
allImageNames = fmap (++)
  (show <$> [Beige .. Black]) <*>
  (show <$> [Queen .. Ant])

-- | Загрузка изображения фишки в нужном масштабе.
loadPieceImage :: String -> IO Picture
loadPieceImage s = fmap (translate 0 0 . scale k k)
  (fold <$> (loadJuicyPNG path))
  where
    path = "images/" ++ s ++ ".png"
    k = 5 / 4 * fromIntegral cellSizeX / (fromIntegral pieceWidth)


-- | Загрузка изображений всех фишек в нужном масштабе.
loadImages :: IO [Picture]
loadImages = sequenceA $ loadPieceImage <$> allImageNames


-- =========================================
-- Инициализация
-- =========================================

--newInitGame :: Game       -- чистая игра (для отладки)
--newInitGame = Game
--  { gameBoard  = createPieces []    -- игровое поле - пусто
--  , gamePlayer = Beige    -- первый игрок ходит бежевыми
--  , gameMovable = Nothing    -- фишка пока что не перемещается
--  , gameEnding = Nothing    -- игра не окончена
--  , gameStepBlack = First -- первый ход черного
--  , gameStepBeige = First -- первый ход бежевого
--  }

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

-- | Инициализация ! после загрузки !
initNewGame :: SaveGame -> IO Game
initNewGame game = newGameWithImages game <$> loadImages   
  
-- | Инициализировать экран с заданными изображениями ! после загрузки !
newGameWithImages :: SaveGame -> [Picture] -> Game
newGameWithImages game images = Game
  { gameBoard  = Map.fromList $ addPicturesInBoard images (board game) []
  , gamePlayer = player game
  , gameMovable = addPicturesInMovable images (movable game)
  , gameEnding = ending game
  , gameStepBlack = stepBlack game
  , gameStepBeige = stepBeige game
  } 
  
addPicturesInMovable :: [Picture] -> Maybe ((Int, Int),(Player, Insect)) -> Maybe ((Int, Int),(Player, Insect, Picture))
addPicturesInMovable _ Nothing = Nothing 
addPicturesInMovable images (Just ((i, j), (pl, ins))) = (Just ((i, j), (pl, ins, takePic images (numberImage pl ins))))
  
addPicturesInBoard :: [Picture] -> [((Int, Int), [(Player, Insect)])] -> [((Int, Int), [(Player, Insect, Picture)])] -> [((Int, Int), [(Player, Insect, Picture)])]
addPicturesInBoard images (x:xs) tmp = if (null x) then tmp
                                else (addPicturesInBoard images xs (newTmp x tmp))
                                  where newTmp ((i, j), [(pl, ins)]) list = ((i, j), [(pl, ins, takePic images (numberImage pl ins))]):list
                                        newTmp _ list = list
addPicturesInBoard _ _ tmp = tmp

numberImage :: Player -> Insect -> Int
numberImage pl ins 
              | pl == Beige && ins == Queen = 0
              | pl == Beige && ins == Spider = 1
              | pl == Beige && ins == Beetle = 2
              | pl == Beige && ins == Hopper = 3
              | pl == Beige && ins == Ant = 4
              | pl == Black && ins == Queen = 5
              | pl == Black && ins == Spider = 6
              | pl == Black && ins == Beetle = 7
              | pl == Black && ins == Hopper = 8
              | otherwise = 9

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

