module MusicName where

data MusicName = BattleTheme
               | CenterTheme
               | DitfyTheme
               | FieldTheme
               | MainTheme
               | TitleTheme
               | TownTheme
               | VictoryTheme
               | VictoryIntro
               | WildBattleIntro
               | WildBattleTheme
    deriving (Enum, Eq, Ord, Read, Show)
