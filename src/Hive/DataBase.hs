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
import Data.Map (toList, fromList)
import Graphics.Gloss.Data.Picture (Picture)
import Data.Typeable
--import           System.Environment   (getArgs)

--type Message = String
--data Database = Database [Message]

data SaveGame = SaveGame { board :: [((Int, Int), [(Player, Insect)])]
                         , player :: Player
                         , movable :: Maybe ((Int, Int), (Player, Insect))
                         , ending :: Maybe Ending
                         , stepBeige :: Step
                         , stepBlack :: Step
                         } deriving (Show, Typeable)

data Database = Database { all :: [SaveGame] }
  deriving (Typeable)
 

$(deriveSafeCopy 0 'base ''Insect)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Ending)
$(deriveSafeCopy 0 'base ''Step)
$(deriveSafeCopy 0 'base ''SaveGame)
$(deriveSafeCopy 0 'base ''Database)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.
addGame :: SaveGame -> Update Database ()
addGame msg
    = do Database messages <- get
         put $ Database (msg:messages)

--viewMessages :: Int -> Query Database [SaveGame]
--viewMessages limit
--    = do Database messages <- ask
--         return $ take limit messages

getGame :: Query Database SaveGame
getGame 
    = do Database messages <- ask
         return $ head (take 1 messages)

-- This will define @GetGame@ and @AddGame@ for us.
$(makeAcidic ''Database ['addGame, 'getGame])

modify :: Game -> SaveGame
modify game = SaveGame { board = delPicturesInBoard (toList (gameBoard game)) []
                    , player = gamePlayer game
                    , movable = delPicturesInMovable (gameMovable game)
                    , ending = gameEnding game
                    , stepBeige = gameStepBeige game
                    , stepBlack = gameStepBlack game
                    } 

delPicturesInBoard :: [((Int, Int), [(Player, Insect, Picture)])] -> [((Int, Int), [(Player, Insect)])] -> [((Int, Int), [(Player, Insect)])]
delPicturesInBoard (x:xs) tmp = if (null x) then tmp
                                else (delPicturesInBoard xs (newTmp x tmp))
                                  where newTmp ((i, j), [(pl1, ins1, _), (pl2, ins2, _), (pl3, ins3, _), (pl4, ins4, _), (pl5, ins5, _)]) list 
                                             = ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3), (pl4, ins4), (pl5, ins5)]):list
                                        newTmp ((i, j), [(pl1, ins1, _), (pl2, ins2, _), (pl3, ins3, _), (pl4, ins4, _)]) list = ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3), (pl4, ins4)]):list
                                        newTmp ((i, j), [(pl1, ins1, _), (pl2, ins2, _), (pl3, ins3, _)]) list = ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3)]):list
                                        newTmp ((i, j), [(pl1, ins1, _), (pl2, ins2, _)]) list = ((i, j), [(pl1, ins1),(pl2, ins2)]):list
                                        newTmp ((i, j), [(pl1, ins1, _)]) list = ((i, j), [(pl1, ins1)]):list
                                        newTmp _ list = list
delPicturesInBoard _ tmp = tmp

delPicturesInMovable :: Maybe ((Int, Int),(Player, Insect, Picture)) -> Maybe ((Int, Int),(Player, Insect))
delPicturesInMovable Nothing = Nothing 
delPicturesInMovable (Just ((i, j), (pl, ins, _))) = (Just ((i, j), (pl, ins)))

saveGame :: Game -> IO()
saveGame game = do { --args <- getArgs
                        putStrLn "Game saved.";
                        database <- openLocalStateFrom "HiveDatabase/" (Database []);
                        update database (AddGame (modify game));
                        gameInBase <- query database GetGame;
                        putStrLn "Last save game:";
                        mapM_ putStrLn [ "board " ++ (show (board gameInBase))];
                        mapM_ putStrLn [ "player " ++ (show (player gameInBase)) ++ " stepBlack " ++ (show (stepBlack gameInBase)) ++ " stepBeige " ++ (show (stepBeige gameInBase)) ];
                        closeAcidState database
--                        if null args
--                        then do messages <- query database (ViewMessages 10)
--                                putStrLn "Last 10 messages:"
--                                mapM_ putStrLn [ "  " ++ message | message <- messages ]
--                        else do update database (AddGame ("ddddddddd"))
--                                putStrLn "Your message has been added to the database."
                      }

loadGame :: IO Game
loadGame = do { putStrLn "Game load.";
                database <- openLocalStateFrom "HiveDatabase/" (Database []);
                game <- query database GetGame;
                closeAcidState database;
                initNewGame game
              }

-- | Инициализация ! после загрузки !
initNewGame :: SaveGame -> IO Game
initNewGame game = newGameWithImages game <$> loadImages   
  
-- | Инициализировать экран с заданными изображениями ! после загрузки !
newGameWithImages :: SaveGame -> [Picture] -> Game
newGameWithImages game images = Game
  { gameBoard  = fromList $ addPicturesInBoard images (board game) []
  , gamePlayer = player game
  , gameMovable = addPicturesInMovable images (movable game)
  , gameEnding = ending game
  , gameStepBlack = stepBlack game
  , gameStepBeige = stepBeige game
  } 

--Возвращение картинки в тип Movable после загрузки игры из базы
addPicturesInMovable :: [Picture] -> Maybe ((Int, Int),(Player, Insect)) -> Maybe ((Int, Int),(Player, Insect, Picture))
addPicturesInMovable _ Nothing = Nothing 
addPicturesInMovable images (Just ((i, j), (pl, ins))) = (Just ((i, j), (pl, ins, takePic images (numberImage pl ins))))

--Возвращение картинки в тип Board после загрузки игры из базы  
addPicturesInBoard :: [Picture] -> [((Int, Int), [(Player, Insect)])] -> [((Int, Int), [(Player, Insect, Picture)])] -> [((Int, Int), [(Player, Insect, Picture)])]
addPicturesInBoard images (x:xs) tmp = if (null x) then tmp
                                else (addPicturesInBoard images xs (newTmp x tmp))
                                  where newTmp ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3), (pl4, ins4), (pl5, ins5)]) list = ((i, j), [(pl1, ins1, takePic images (numberImage pl1 ins1)), (pl2, ins2, takePic images (numberImage pl2 ins2)), (pl3, ins3, takePic images (numberImage pl3 ins3)), (pl4, ins4, takePic images (numberImage pl4 ins4)), (pl5, ins5, takePic images (numberImage pl5 ins5))]):list
                                        newTmp ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3), (pl4, ins4)]) list = ((i, j), [(pl1, ins1, takePic images (numberImage pl1 ins1)), (pl2, ins2, takePic images (numberImage pl2 ins2)), (pl3, ins3, takePic images (numberImage pl3 ins3)), (pl4, ins4, takePic images (numberImage pl4 ins4))]):list
                                        newTmp ((i, j), [(pl1, ins1), (pl2, ins2), (pl3, ins3)]) list = ((i, j), [(pl1, ins1, takePic images (numberImage pl1 ins1)), (pl2, ins2, takePic images (numberImage pl2 ins2)), (pl3, ins3, takePic images (numberImage pl3 ins3))]):list
                                        newTmp ((i, j), [(pl1, ins1), (pl2, ins2)]) list = ((i, j), [(pl1, ins1, takePic images (numberImage pl1 ins1)), (pl2, ins2, takePic images (numberImage pl2 ins2))]):list
                                        newTmp ((i, j), [(pl1, ins1)]) list = ((i, j), [(pl1, ins1, takePic images (numberImage pl1 ins1))]):list
                                        newTmp _ list = list
addPicturesInBoard _ _ tmp = tmp

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