import Data.Vect
import Data.StateStore
import InputOutput.Interact


partial
main : IO ()
main = do (Just _, state) <-
            run forever initState interact
                | _ => putStrLn "Ran Out of energy"
          putStrLn ("Final State: " ++ show state)
