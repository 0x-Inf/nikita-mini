import Data.Vect

import NikitaCore.CoreData
import DynamicalSystemPrims.DynamicalSystem


%access public export

||| This is the type of the mode-dependent interface of the machine
record NikitaArena where
       constructor MkNikitaArena
       ||| input
       pos : Type
       ||| output
       direction : pos -> Type
  
