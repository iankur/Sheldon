module Move where

import Field
import Figure

data Move = RegularMove   { from :: Field, to :: Field }
          | PromotionMove { from :: Field, to :: Field, figure :: Figure }
          | EnPassantMove { from :: Field, to :: Field, captured :: Field }
          | CastlingMove  { from :: Field, to :: Field, rookFrom :: Field, rookTo :: Field }
          | NullMove
  deriving (Show, Eq)

{-

ghci
:load Chess/Move.hs
:q

-}
