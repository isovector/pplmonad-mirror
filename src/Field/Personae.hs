module Field.Personae (
    module Field.Personae.Donald,
    module Field.Personae.Kid,
    module Field.Personae.Leaner,
    module Field.Personae.Man,
    module Field.Personae.Mom,
    module Field.Personae.Protagonist,
    module Field.Personae.Woke,
    personaByName
) where

import Field.Personae.Donald
import Field.Personae.Kid
import Field.Personae.Leaner
import Field.Personae.Man
import Field.Personae.Mom
import Field.Personae.Protagonist
import Field.Personae.Woke
import Field.PersonaName

personaByName Donald = donald
personaByName Kid = kid
personaByName Leaner = leaner
personaByName Man = man
personaByName Mom = mom
personaByName Protagonist = protagonist
personaByName Woke = msWoke
