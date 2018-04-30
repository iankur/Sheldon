module FigureMoves where

import Properties

-- | Sequences of relative figure positions for rook moves.
rookMoves = [([1..],repeat 0),
             ([-1,-2..],repeat 0),
             (repeat 0,[1..]),
             (repeat 0, [-1,-2..])]

-- | Sequences of relative figure positions for bishop moves.
bishopMoves = [([1..],[1..]),
               ([-1,-2..],[1..]),
               ([1..],[-1,-2..]),
               ([-1,-2..],[-1,-2..])]

-- | Sequences of relative figure positions for queen moves.
queenMoves = rookMoves ++ bishopMoves

-- | Sequences of relative figure positions for knight moves.
knightMoves = [([1],[2]),
               ([2],[1]),
               ([-1],[2]),
               ([2],[-1]),
               ([-1],[-2]),
               ([-2],[-1]),
               ([1],[-2]),
               ([-2],[1])]

-- | Sequences of relative figure positions for king moves.
kingMoves = map (\(c,r) -> (take 1 c, take 1 r)) queenMoves

-- | Choose the sequences of relative figure positions
-- based on the figure position, type, color,
-- and whether the move is a capture move or not.s
chooseFigureMoves :: Figure -> Field -> Bool -> [([Int],[Int])]
chooseFigureMoves (Figure Rook _)     f             _     = rookMoves
chooseFigureMoves (Figure Bishop _)   f             _     = bishopMoves
chooseFigureMoves (Figure King _)     f             _     = kingMoves
chooseFigureMoves (Figure Queen _)    f             _     = queenMoves
chooseFigureMoves (Figure Knight _)   f             _     = knightMoves
chooseFigureMoves (Figure Pawn White) f@(Field _ 2) False = [(repeat 0,[1,2])]
chooseFigureMoves (Figure Pawn White) f@(Field _ _) False = [([0],[1])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ 7) False = [(repeat 0,[-1,-2])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ _) False = [([0],[-1])] 
chooseFigureMoves (Figure Pawn White) f@(Field _ _) True  = [([-1],[1]),([1],[1])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ _) True  = [([-1],[-1]),([1],[-1])]

-- | Returns the field relative to the given field according to
-- a pair of relative coordinates.
relativeField (Field col row) (c,r) = Field (col+c) (row+r)

-- | Returns fields relative to the given field according to
-- the sequence of relative coordinates.
relativeFields field (cols,rows) =
  takeWhile isValid (map (relativeField field) (zip cols rows))

-- | Returns possible figure moves.
-- The figure is on the field 'field' and the 'capture' flag indicate whether
-- the move is a capture.
figureMoves :: Figure -> Field -> Bool -> [[Field]]
figureMoves figure field capture = map (relativeFields field) $ chooseFigureMoves figure field capture
