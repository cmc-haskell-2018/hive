{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hive.DataBase
  where

import Hive.Model
import Hive.Init

import Graphics.Gloss.Data.Picture
import Data.Acid
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.SafeCopy
import Data.Map as Map (toList, fromList)
import Graphics.Gloss.Data.Picture (Picture)
import Data.Typeable

data SaveGame = SaveGame { board :: [(Coord, [(Player, Insect)])]
                         , player :: Player
                         , movable :: Maybe (Coord, (Player, Insect))
                         , ending :: Maybe Ending
                         , stepBeige :: Step
                         , stepBlack :: Step
                         , userName :: String
                         } deriving (Show, Typeable, Eq)

data Database = Database { all :: [SaveGame] }
  deriving (Typeable) 
 
$(deriveSafeCopy 0 'base ''Insect)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Ending)
$(deriveSafeCopy 0 'base ''Step)
$(deriveSafeCopy 0 'base ''SaveGame)
$(deriveSafeCopy 0 'base ''Database)

addGame :: SaveGame -> Update Database ()
addGame msg
    = do Database messages <- get
         put $ Database (msg:messages)

getGame :: String -> Query Database SaveGame
getGame name
    = do Database messages <- ask;
         return (case filter (\x -> (userName x) == name) messages of
                  [] -> SaveGame { board = [], player = Beige, movable = Nothing, ending = Nothing, stepBeige = First, stepBlack = First, userName = "" } 
                  (x:_) -> x)

$(makeAcidic ''Database ['addGame, 'getGame])

modify :: Game -> SaveGame
modify game = SaveGame { board = delPicturesInBoard (gameBoard game)
                    , player = gamePlayer game
                    , movable = fmap delPicturesInMovable (gameMovable game)
                    , ending = gameEnding game
                    , stepBeige = gameStepBeige game
                    , stepBlack = gameStepBlack game
                    , userName = gameUserName game
                    } 

delPicturesInPiece :: Piece -> (Player, Insect)
delPicturesInPiece (pl, ins, _ ) = (pl, ins)

delPicturesInMovable :: Movable -> (Coord,(Player, Insect))
delPicturesInMovable (coord, piece) = (coord, delPicturesInPiece piece)

delPicturesInCell :: (Coord, Cell) -> (Coord, [(Player, Insect)])
delPicturesInCell (coord, cell) = (coord, map delPicturesInPiece cell)

delPicturesInBoard :: Board -> [(Coord, [(Player, Insect)])]
delPicturesInBoard currentBoard = map delPicturesInCell (Map.toList currentBoard)

saveGame :: Game -> IO()
saveGame game = do { database <- openLocalStateFrom "HiveDatabase/" (Database []);
                     update database (AddGame (modify game));
                     gameInBase <- query database (GetGame (gameUserName game));
                     putStrLn ("Game saved. User name: " ++ show (userName gameInBase));
--                   putStrLn "Last save game:";
--                   mapM_ putStrLn [ "board " ++ (show (board gameInBase))];
--                   mapM_ putStrLn [ "user_name " ++ (show (userName gameInBase)) ++ " player " ++ (show (player gameInBase)) ++ " stepBlack " ++ (show (stepBlack gameInBase)) ++ " stepBeige " ++ (show (stepBeige gameInBase)) ];
                     closeAcidState database
                   }

loadGame :: String -> IO Game
loadGame name = do { database <- openLocalStateFrom "HiveDatabase/" (Database []);
                     game <- query database (GetGame name);
                     closeAcidState database;
                     putStrLn "Game load.";
                     initNewGame game;
                   }

-- | Инициализация ! после загрузки !
initNewGame :: SaveGame -> IO Game
initNewGame game = newGameWithImages game <$> loadImages   
  
-- | Инициализировать экран с заданными изображениями ! после загрузки !
newGameWithImages :: SaveGame -> [Picture] -> Game
newGameWithImages game images = Game
  { gameBoard  = addPicturesInBoard images (board game)
  , gamePlayer = player game
  , gameMovable = addPicturesInMovable images (movable game)
  , gameEnding = ending game
  , gameStepBlack = stepBlack game
  , gameStepBeige = stepBeige game
  , gameUserName = userName game
  } 

addPicturesInPiece :: [Picture] -> (Player, Insect) -> Piece 
addPicturesInPiece images (pl, ins) = (pl, ins, takePic images (numberImage pl ins))

addPicturesInMovable :: [Picture] -> Maybe (Coord,(Player, Insect)) -> Maybe Movable
addPicturesInMovable images (Just (coord, piece)) = Just (coord, addPicturesInPiece images piece)
addPicturesInMovable _ Nothing = Nothing

addPicturesInCell :: [Picture] ->  (Coord, [(Player, Insect)]) -> (Coord, Cell)
addPicturesInCell images (coord, pieces) = (coord, map (addPicturesInPiece images) pieces)

addPicturesInBoard :: [Picture] -> [(Coord, [(Player, Insect)])] -> Board
addPicturesInBoard images loadBoard = Map.fromList $ map (addPicturesInCell images) loadBoard

--Номер для выбора необходимой картинки
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

-- Взять картинку из списка по номеру (кажется, такой подход абсолютно отвратителен, но я не уверена)
takePic :: [Picture] -> Int -> Picture
takePic [] _ = blank
takePic (p : ps) n
    | n < 0 = blank
    | n == 0 = p
    | otherwise = takePic ps (n - 1)