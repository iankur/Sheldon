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

{-

ghci
:load Chess/Figure.hs
Figure King White -- k
Figure King Black -- K
Figure Queen Black -- Q
Figure Rook Black -- R
:q

-}
