module Battle.Moves (
    module Battle.Moves.EyePoke,
    module Battle.Moves.MeToo,
    module Battle.Moves.SuckerPunch,
    enemyMoveByName,
    friendMoveByName
) where

import Battle.MoveName
import Battle.Moves.EyePoke
import Battle.Moves.MeToo
import Battle.Moves.SuckerPunch

friendMoveByName EyePoke     = eyePokeFriend
friendMoveByName MeToo       = meTooFriend
friendMoveByName SuckerPunch = suckerPunchFriend

enemyMoveByName EyePoke     = eyePokeEnemy
enemyMoveByName MeToo       = meTooEnemy
enemyMoveByName SuckerPunch = suckerPunchFriend
