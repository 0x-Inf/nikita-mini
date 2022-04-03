import Data.Vect
import Data.StateStore
import InputOutput.Interact


main : IO ()
main = replWith (MkInteractState _ []) "Nikita: " processInteraction
