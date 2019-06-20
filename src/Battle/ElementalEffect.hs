module Battle.ElementalEffect where

import Data.Array

import Ppmn.Parameters

data Effectiveness = Futile | Weak | Basic | Strong
    deriving (Enum, Eq)

instance Semigroup Effectiveness where
    Futile <> _      = Futile
    _      <> Futile = Futile
    Weak   <> Strong = Basic
    Strong <> Weak   = Basic
    Strong <> _      = Strong
    _      <> Strong = Strong
    x      <> y      = toEnum $ clamp ((fromEnum x + fromEnum y) `div` 2)
      where
        clamp = max (fromEnum Futile) . min (fromEnum Strong)

instance Monoid Effectiveness where
    mempty = Basic

elemEffects = listArray ((1,1), (6,6)) $ concat [rNormal, rAuthority, rApathy, rSpite, rWit, rGreed]
  where
    rNormal    = [ Basic,  Basic,  Basic,  Basic,  Basic,  Basic  ]
    rAuthority = [ Strong, Basic,  Weak,   Basic,  Basic,  Basic  ]
    rApathy    = [ Basic,  Basic,  Basic,  Strong, Weak,   Basic  ]
    rWit       = [ Basic,  Basic,  Weak,   Basic,  Strong, Strong ]
    rSpite     = [ Basic,  Basic,  Strong, Weak,   Basic,  Weak   ]
    rGreed     = [ Strong, Basic,  Strong, Basic,  Basic,  Basic  ]

effectiveness :: Element -> Element -> Effectiveness
effectiveness move defend = elemEffects ! ((fromEnum move + 1), (fromEnum defend + 1))
