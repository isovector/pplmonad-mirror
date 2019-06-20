module Field.CardinalDirection where

data CardinalDirection = East | North | South | West
    deriving (Enum, Eq, Ord, Read, Show)

oppositeDirection North = South
oppositeDirection West  = East
oppositeDirection South = North
oppositeDirection East  = West
