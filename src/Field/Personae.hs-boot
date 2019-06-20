module Field.Personae where

import {-# SOURCE #-} Field.Character
import Field.PersonaName

personaByName :: PersonaName -> (Int, Int) -> Character
