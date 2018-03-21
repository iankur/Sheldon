module Figure where

import Color

-- | Represents chess figure types.
data FigureType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Show,Eq,Ord)

-- | Represents a figure, which has a type and a color.
data Figure = Figure { figureType :: FigureType, figureColor :: Color }
  deriving (Eq,Ord)

instance Show Figure where
  -- | Returns a one-character string representing the figure.
  show (Figure King   White) = "king"
  show (Figure Queen  White) = "queen"
  show (Figure Rook   White) = "rook"
  show (Figure Bishop White) = "bishop"
  show (Figure Knight White) = "knight"
  show (Figure Pawn   White) = "pawn"
  show (Figure King   Black) = "KING"
  show (Figure Queen  Black) = "QUEEN"
  show (Figure Rook   Black) = "ROOK"
  show (Figure Bishop Black) = "BISHOP"
  show (Figure Knight Black) = "KNIGHT"
  show (Figure Pawn   Black) = "PAWN"

{-

ghci
:load Chess/Figure.hs
Figure King White -- k
Figure King Black -- K
Figure Queen Black -- Q
Figure Rook Black -- R
:q

-}
