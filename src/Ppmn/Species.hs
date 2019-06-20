module Ppmn.Species (
    module Ppmn.Species.Blamotage,
    module Ppmn.Species.Ignoloof,
    module Ppmn.Species.Incub,
    module Ppmn.Species.Slidek,
    module Ppmn.Species.Unner,
    module Ppmn.Species.You,
    ppmnByName
) where

import LabelName
import Ppmn.Species.Blamotage
import Ppmn.Species.Ignoloof
import Ppmn.Species.Incub
import Ppmn.Species.Slidek
import Ppmn.Species.Unner
import Ppmn.Species.You
import Ppmn.Parameters

ppmnByName Blamotage level = atLevel level blamotageLearnMove blamotageBase
ppmnByName Ignoloof level = atLevel level ignoloofLearnMove ignoloofBase
ppmnByName Incub level = atLevel level incubLearnMove incubBase
ppmnByName Slidek level = atLevel level slidekLearnMove slidekBase
ppmnByName Unner level = atLevel level unnerLearnMove unnerBase
ppmnByName You level = atLevel level youLearnMove youBase
