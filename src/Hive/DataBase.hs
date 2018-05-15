{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hive.DataBase
  where

import Hive.Model
  
import Data.Acid

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.SafeCopy
import Data.Map (toList)
import Graphics.Gloss.Data.Picture
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
                                  where newTmp ((i, j), [(pl, ins, _)]) list = ((i, j), [(pl, ins)]):list
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
--                        mapM_ putStrLn [ "board " ++ (show (board message)) ++ "player " ++ (show (player message)) ++ " stepBlack " ++ (show (stepBlack message)) ++ " stepBeige " ++ (show (stepBeige message)) | message <- messages ];
                        closeAcidState database
--                        if null args
--                        then do messages <- query database (ViewMessages 10)
--                                putStrLn "Last 10 messages:"
--                                mapM_ putStrLn [ "  " ++ message | message <- messages ]
--                        else do update database (AddGame ("ddddddddd"))
--                                putStrLn "Your message has been added to the database."
                      }

loadGame :: Game -> IO()
loadGame _ = do { putStrLn "Game load.";
                     database <- openLocalStateFrom "HiveDatabase/" (Database []);
--                     game <- query database GetGame;
                     closeAcidState database;
                   }
