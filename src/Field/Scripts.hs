module Field.Scripts (
    module Field.Scripts.BossRoom,
    module Field.Scripts.Courtyard,
    module Field.Scripts.Ditfy,
    module Field.Scripts.FamilyHouse1F,
    module Field.Scripts.FamilyHouse2F,
    module Field.Scripts.Hometown,
    module Field.Scripts.WeWoke,
    scriptByMap
) where

import Field.Locale
import Field.MapName
import Field.Scripts.BossRoom
import Field.Scripts.Courtyard
import Field.Scripts.Ditfy
import Field.Scripts.FamilyHouse1F
import Field.Scripts.FamilyHouse2F
import Field.Scripts.Hometown
import Field.Scripts.WeWoke
import MusicName

scriptByMap  BossRoom       t  _ = bossRoom t
scriptByMap  Courtyard      t  _ = courtyard t
scriptByMap  Ditfy          t  r = ditfy t r
scriptByMap  FamilyHouse1F  t  _ = familyHouse1FInitial t
scriptByMap  FamilyHouse2F  t  _ = familyHouse2F t
scriptByMap  Hometown       t  _ = hometownInitial t
scriptByMap  WeWoke         t  r = weWokeInitial t r
scriptByMap  _              t  _ = stdLocale [] FieldTheme t
