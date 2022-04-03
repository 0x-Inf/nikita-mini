module CoreData

import Data.Vect
%access public export

record CoreState where
       constructor MkNikitaCoreState
       size : Nat
       current : Vect n String
       state_rep : Vect size (Vect n String)
