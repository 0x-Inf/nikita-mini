module Interact

import Data.Vect
import Data.Primitives.Views
import System

import NikitaCore.Conversation
import NikitaCore.CoreData

%access public export
%default total

record InteractState where
       constructor MkInteractState
       momentEntropy : Int
       totalEntropy : Int
       mContentSize : Nat
       tContentSize : Nat
       momentContent : Vect mContentSize String  -- This should ideally be a graph
       totalContent : Vect tContentSize Type   -- This too
       repository : List String

Show InteractState where
  show st = "The total Interact Entropy: " ++ show (totalEntropy st) ++ "\n" ++
            "The current total Content size: " ++ show (tContentSize st) ++ "\n" ++
            "The moment content size: " ++ show (mContentSize st)



data Command : Type -> Type where                                               -- Defines the valid commands for an interactive program
    PutStr : String -> Command ()
    PutStrLn : String -> Command ()
    GetLine : Command String

    GetEnvEntropy : Command Integer
    GetInteractiveState : Command InteractState
    PutInteractiveState : InteractState -> Command ()

    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where                                     -- Interactive programs that either produce result with Quit or loop indefinitely
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
     (>>=) : Command a -> (a -> Command b) -> Command b                -- Kleisli composition to support the do notation
     (>>=) = Bind

namespace ConsoleDo
     (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
     (>>=) = Do

data Input = Prompt String                                         -- An input is either a string that prompts the machine or a quit command
           | QuitCmd

data Energy = Dry | More (Lazy Energy)


partial
forever : Energy
forever = More forever


runCommand : InteractState -> Command a -> IO (a, InteractState)
runCommand state (PutStr x) = do putStr x
                                 pure ((), state)
runCommand state (PutStrLn x) = do putStrLn x
                                   pure ((), state)
runCommand state GetLine = do str <- getLine
                              pure (str, state)
runCommand state GetEnvEntropy = do timeEntr <- time
                                    pure (timeEntr, state)
runCommand state GetInteractiveState
    = pure (state, state)
runCommand state (PutInteractiveState newState)
    = pure ((), state)
runCommand state (Pure val) = pure (val, state)
runCommand state (Bind c f)
    = do (res, newState) <- runCommand state c
         runCommand newState (f res)



run : Energy -> InteractState -> ConsoleIO a -> IO (Maybe a, InteractState)
run Dry state (Quit x) = pure (Nothing, state)
run energy state (Quit val) = do pure (Just val, state)
run (More energy) state (Do c f)
    = do (res, newState) <- runCommand state c
         run energy newState (f res)


initState : InteractState
initState = MkInteractState 1 2 0 0 [] [] []

setTotalEntropy : Int -> InteractState -> InteractState
setTotalEntropy newTotalEntropy state = record { totalEntropy = newTotalEntropy} state

addInteraction : String ->  InteractState -> InteractState
addInteraction update state = ?addInteraction_rhs


mutual
  processPrompt : String -> ConsoleIO InteractState
  processPrompt x = let response = processConversationInput x in
                        do PutStrLn response
                           st <- GetInteractiveState
                           PutInteractiveState (addInteraction response st)
                           interact

  readInput : (prompt : String) -> Command Input
  readInput prompt = do PutStr prompt
                        answer <- GetLine
                        if toLower answer == "bye"
                           then Pure QuitCmd
                           else Pure (Prompt (answer))


  interact : ConsoleIO InteractState
  interact = do envEntr <- GetEnvEntropy
                state <- GetInteractiveState
                PutStr (show state ++ "\n")

                input <- readInput ("You:")
                case input of
                     (Prompt x) => processPrompt x
                     QuitCmd => Quit state




-- import Data.Vect
--
-- import NikitaCore.Conversation
-- import NikitaCore.CoreData
-- %access public export
--
-- -- public export
-- record InteractState where
--        constructor MkInteractState
--        size : Nat
--        items : Vect size String  -- Vec n String is better
--
--
-- updateNikitaState : (state : InteractState) -> String -> InteractState
-- updateNikitaState (MkInteractState size state) newStateItem = MkInteractState _ (addToStateData state)
--   where
--     addToStateData : Vect oldsize String -> Vect (S oldsize) String
--     addToStateData [] = [newStateItem]
--     addToStateData (item :: items) = item :: addToStateData items
--
-- updateCoreState : (core_state : CoreState) -> (interaction_state : InteractState) -> CoreState
-- updateCoreState (MkNikitaCoreState size current state_rep) (MkInteractState k items) = ?updateCoreState_rhs_2
--
-- -- public export
-- data InteractOption : Type where
--      Oracle       : String -> InteractOption
--      Conversation : String -> InteractOption
--      Recall       : Integer -> InteractOption
--      Bye          : InteractOption
--
--
-- parseCommand : (cmd : String) -> (args : String) -> Maybe InteractOption
-- parseCommand "oracle" str = Just (Oracle str)
-- parseCommand "conv" str = Just (Conversation str)
-- parseCommand "recall" val = case all isDigit (unpack val) of
--                                  False => Nothing
--                                  True => Just (Recall (cast val))
-- parseCommand "bye" ""     = Just Bye
-- parseCommand _ _          = Nothing
--
-- parse : (input : String) -> Maybe InteractOption
-- parse input = case span (/= ' ') input of
--                    (cmd, args) => parseCommand cmd (ltrim args)
--
--
-- recallFunction : (pos : Integer) -> (state : InteractState) -> Maybe (String, InteractState)
-- recallFunction pos state
--   = let state_items = items state in
--         case integerToFin pos (size state) of
--              Nothing => Just ("Out of range \n", state)
--              (Just id) => Just (index id state_items ++ "\n", state)
--
-- nikitaProcessing : (state : InteractState) -> (item : String) -> String
-- nikitaProcessing state item = processConversationInput item
--   -- let words_in = words item in
--   --     show (length words_in)
--
--
-- oracleProcessing : (item : String) -> String
-- oracleProcessing item = "Oracle has left!"
--
-- -- export
-- processInteraction : InteractState -> String -> Maybe (String, InteractState)
-- processInteraction state input
--   = case parse input of
--          Nothing => Just ("Invalid command \n", state)
--          Just (Oracle item) => Just ( oracleProcessing item ++ "\n" ++ "Oracle Entry " ++ show (size state) ++ "\n", updateNikitaState state item)
--          Just (Conversation item) => Just (nikitaProcessing state item ++ "\n" ++ "Conversation with ID" ++ show (size state) ++ "\n", updateNikitaState state item)
--          Just (Recall pos)  => recallFunction pos state
--          Just Bye          => Nothing
