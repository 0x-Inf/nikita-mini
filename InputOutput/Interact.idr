module Interact

import Data.Vect

import NikitaCore.Conversation
import NikitaCore.CoreData
%access public export

-- public export
record InteractState where
       constructor MkInteractState
       size : Nat
       items : Vect size String  -- Vec n String is better


updateNikitaState : (state : InteractState) -> String -> InteractState
updateNikitaState (MkInteractState size state) newStateItem = MkInteractState _ (addToStateData state)
  where
    addToStateData : Vect oldsize String -> Vect (S oldsize) String
    addToStateData [] = [newStateItem]
    addToStateData (item :: items) = item :: addToStateData items



-- public export
data InteractOption : Type where
     Oracle       : String -> InteractOption
     Conversation : String -> InteractOption
     Recall       : Integer -> InteractOption
     Bye          : InteractOption


parseCommand : (cmd : String) -> (args : String) -> Maybe InteractOption
parseCommand "oracle" str = Just (Oracle str)
parseCommand "conv" str = Just (Conversation str)
parseCommand "recall" val = case all isDigit (unpack val) of
                                 False => Nothing
                                 True => Just (Recall (cast val))
parseCommand "bye" ""     = Just Bye
parseCommand _ _          = Nothing

parse : (input : String) -> Maybe InteractOption
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


recallFunction : (pos : Integer) -> (state : InteractState) -> Maybe (String, InteractState)
recallFunction pos state
  = let state_items = items state in
        case integerToFin pos (size state) of
             Nothing => Just ("Out of range \n", state)
             (Just id) => Just (index id state_items ++ "\n", state)

nikitaProcessing : (state : InteractState) -> (item : String) -> String
nikitaProcessing state item = processConversationInput item
  -- let words_in = words item in
  --     show (length words_in)


oracleProcessing : (item : String) -> String
oracleProcessing item = "Oracle has left!"

-- export
processInteraction : InteractState -> String -> Maybe (String, InteractState)
processInteraction state input
  = case parse input of
         Nothing => Just ("Invalid command \n", state)
         Just (Oracle item) => Just ( oracleProcessing item ++ "\n" ++ "Oracle Entry " ++ show (size state) ++ "\n", updateNikitaState state item)
         Just (Conversation item) => Just (nikitaProcessing state item ++ "\n" ++ "Conversation with ID" ++ show (size state) ++ "\n", updateNikitaState state item)
         Just (Recall pos)  => recallFunction pos state
         Just Bye          => Nothing
