module Main where

import Hive
import Graphics.Gloss.Interface.IO.Game
import Data.Acid

main :: IO ()
main = do game <- initGame;
          playIO display bgColor fps game drawGame handleGame updateGame
            where
             display = InWindow "Hive" (screenWidth, screenHeight) (0, 0)
             bgColor = white   -- цвет фона
             fps     = 0      -- кол-во кадров в секунду

-- | Обработка нажатия клавиш мыши
handleGame :: Event -> Game -> IO Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game
  | gameEnding game /= Nothing = return $ game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = return $ takePiece coord game    -- фишка еще не взята
  | otherwise = return $ makeMove coord game    -- фишка уже взята
          where
            coord = getCell mouse
handleGame (EventKey (MouseButton RightButton) Down _ _) game       -- положить фишку обратно
  | gameEnding game /= Nothing = return $ game    -- если игра окончена, ничего сделать нельзя
  | gameMovable game == Nothing = return $ game    -- фишка еще не взята, отменять нечего
  | otherwise = return $ putPieceBack game       -- фишка взята, кладем ее на место
handleGame (EventKey (SpecialKey KeyEnter) Down _ _) game = do saveGame game; return game -- сохранить игру
handleGame (EventKey (SpecialKey KeyTab) Down _ _) gameOld = do putStrLn "Game load.";    -- загрузить последнюю игру 
                                                                database <- openLocalStateFrom "HiveDatabase/" (Database []);
                                                                game <- query database GetGame;
                                                                closeAcidState database;
                                                                return Game { gameBoard = gameBoard gameOld,
                                                                              gamePlayer = player game,
                                                                              gameMovable = gameMovable gameOld,
                                                                              gameEnding = ending game,
                                                                              gameStepBlack = stepBlack game,
                                                                              gameStepBeige = stepBeige game
                                                                            } 
handleGame _ game = return game

-- | Обновление игры.
updateGame :: Float -> Game -> IO Game
updateGame _ = return;

--addPicturesInMovable :: Maybe ((Int, Int),(Player, Insect)) -> Maybe ((Int, Int),(Player, Insect, Picture))
--addPicturesInMovable Nothing = Nothing 
--addPicturesInMovable (Just ((i, j), (pl, ins))) = (Just ((i, j), (pl, ins, (pic pl ins))))
--       where pic :: Player -> Insect -> Picture
--             pic _ _ = getImages <$> loadImages
--             
--             getImages :: [Picture] -> Picture
--             getImages images = takePic images 0