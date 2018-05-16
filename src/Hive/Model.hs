module Hive.Model
  where

import Data.Map (Map)
import Graphics.Gloss.Data.Picture

-- =========================================
-- Модель игры
-- =========================================


-- | Насекомые
data Insect = Queen | Spider | Beetle | Hopper | Ant | LadyBug | Mosquito
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
  deriving (Show, Eq, Ord, Enum)

-- | Окончание игры
data Ending = Win Player | Tie deriving (Eq, Show)

-- | Контроль количества шагов
data Step = First | Second | Third | Fours | Other 
  deriving (Enum, Eq, Show)

-- | Состояние игры
data Game = Game
  { gameBoard  :: Board    -- Игровое поле.
  , gamePlayer :: Player    -- Чей ход?
  , gameMovable :: Maybe Movable  -- Nothing - никакая фишка не перемещается, иначе - указана перемещаемая фишка.
  , gameEnding :: Maybe Ending    -- Nothing - игра не окончена.
  , gameStepBlack :: Step -- Номер хода черного игрока 
  , gameStepBeige :: Step -- Номер хода бежевого игрока 
  } deriving (Show)
