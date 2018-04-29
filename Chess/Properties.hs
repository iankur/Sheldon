module Properties where

import Data.Char

-- | The 'Color' class represents one of the two colors ('Black' or 'White')
-- used in the game of Chess.
data Color = White | Black
  deriving (Eq,Show,Ord)

-- | The 'other' method returns the opposite color.
other :: Color -> Color
other White = Black
other Black = White

-- | The 'firstRow' method returns the coordinate of the first row
-- from the point of view of a player who plays the given color.
firstRow :: Color -> Int
firstRow White = 1
firstRow Black = 8

data Field = Field { col :: Int, row :: Int }
  deriving (Eq, Ord)

-- | Shows field coordinates as a pair of characters:
-- a letter representing the column and a number representing the row.
instance Show Field where
  show f = chr (ord 'a' + (col f) - 1) : show (row f)

-- | Returns a boolean value indicating
-- whether the given field belongs to the last row from
-- the point of view of a player.
isLastRow :: Field -> Color -> Bool
isLastRow (Field _ row) color = row == firstRow (other color)

-- | Returns a boolean value indicating
-- whether the field has valid coordinates, that is
-- whether it belongs to the board.
isValid :: Field -> Bool
isValid (Field col row) = col >= 1 && col <= 8 && row >= 1 && row <= 8

-- | Represents chess figure types.
data FigureType = King | Queen | Rook | Bishop | Knight | Pawn 
  deriving (Show,Eq,Ord)

-- | Represents a figure, which has a type and a color.
data Figure = Figure { figureType :: FigureType, figureColor :: Color }
  deriving (Eq,Ord)

instance Show Figure where
  -- | Returns a one-character string representing the figure.
  show (Figure King   White) = "K"
  show (Figure Queen  White) = "Q"
  show (Figure Rook   White) = "R"
  show (Figure Bishop White) = "B"
  show (Figure Knight White) = "N"
  show (Figure Pawn   White) = "P"
  show (Figure King   Black) = "k"
  show (Figure Queen  Black) = "q"
  show (Figure Rook   Black) = "r"
  show (Figure Bishop Black) = "b"
  show (Figure Knight Black) = "n"
  show (Figure Pawn   Black) = "p"

data Move = RegularMove   { from :: Field, to :: Field }
          | PromotionMove { from :: Field, to :: Field, figure :: Figure }
          -- | EnPassantMove { from :: Field, to :: Field, captured :: Field }
          | CastlingMove  { from :: Field, to :: Field, rookFrom :: Field, rookTo :: Field }
          | NullMove
  deriving (Show, Eq)