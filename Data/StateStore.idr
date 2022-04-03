module StateStore

import Data.Vect


infixr 5 .+.

data Schema = SString -- a schema contraining a single string
            -- | SVect -- a schema contraining a tuple string and an int
            | SInt           -- a schema contraining a single int
            | (.+.) Schema Schema   -- a schema combining two schemas

||| A type level fn for converting a Schema to a concrete type
SchemaType : Schema -> Type
SchemaType SString = String
-- SchemaType SVect = Vect
SchemaType SInt = Int
SchemaType (schemal .+. schemar) = (SchemaType schemal, SchemaType schemar)


||| The record type constructor for the state store data
record StateStore where
       constructor MkState
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

public export
record StateStore' where
       constructor MkState'
       size : Nat
       items : Vect size String


updateStateStore : (state : StateStore) -> SchemaType (schema state) -> StateStore
updateStateStore (MkState schema size state) newItem = MkState schema _ (addToData state)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items


updateStateStore' : StateStore' -> String -> StateStore'
updateStateStore' (MkState' size items) newitem = MkState' _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (item :: itemss) = item :: addToData itemss


data Mode : Schema -> Type where
     Interact : SchemaType schema -> String -> Mode schema
     Analysis : SchemaType schema -> Mode schema
     Retrieve : Integer -> Mode schema
     Exit     : Mode schema


data Mode' : Type where
     Interact' : String  -> Mode'
     Analysis' : String  -> Mode'
     Retrieve' : Integer -> Mode'
     Exit'     : Mode'


parseCommand : (cmd : String) -> (args : String) -> Maybe Mode'
parseCommand "interact" str = Just (Interact' str)
parseCommand "analysis" str = Just (Analysis' str)
parseCommand "retrieve" val = case all isDigit (unpack val) of
                                   False => Nothing
                                   True => Just (Retrieve' (cast val))
parseCommand "exit" "" = Just Exit'
parseCommand _ _       = Nothing

parse : (input : String) -> Maybe Mode'
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr


retrieveState : (pos : Integer) -> (state : StateStore) -> Maybe (String, StateStore)
retrieveState pos state
  = let state_items = items state in
        case integerToFin pos (size state) of
             Nothing   => Just ("Out of range \n", state)
             (Just id) => Just (display (index id state_items) ++ "\n", state)

retrieveState' : (pos : Integer) -> (state : StateStore') -> Maybe (String, StateStore')
retrieveState' pos state
 = let state_items = items state in
       case integerToFin pos (size state) of
            Nothing   => Just ("Out of range \n", state)
            (Just id) => Just (index id state_items ++ "\n", state)

export
processInput : StateStore' -> String -> Maybe (String, StateStore')
processInput state inp
  = case parse inp of
         Nothing => Just ("Invalid command \n", state)
         Just (Interact' item) => Just ("Interaction ID " ++ show (size state) ++ "\n", updateStateStore' state item)
         Just (Analysis' item) => Just ("Analysis ID " ++ show (size state) ++ "\n", updateStateStore' state item)
         Just (Retrieve' pos)  => retrieveState' pos state
         Just Exit'             => Nothing




-- data StateStore' : Type where
--      ||| The canonical way of constructing a state store
--      MkState' : (schema' : Schema) -> (size' : Nat) -> (items' : Vect size (SchemaType schema)) -> StateStore'

-- size : StateStore -> Nat
-- size (MkState size' items') = size'
--
--
-- items : (store : StateStore) -> Vect (size store) (Vect n String)
-- items (MkState size items) = ?items_rhs_1



{-
module Main

import Data.Vect

||| The Type constructor for Data Store
data DataStore : Type where
     ||| A data constructor which gives a canonical was of constructing a data store
     MkData    : (size : Nat) -> (items : Vect size String) -> DataStore


size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (item :: itemss) = item :: addToData itemss

data Command = Add String
             | Get Integer
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)         -- this matches where the first arg is "add". The string you add to the store will be composed of the second arg str
parseCommand "get" val = case all isDigit (unpack val) of -- the parse is successful if the second arg is all digits
                              False => Nothing
                              True  => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _       = Nothing                     -- if none of the previous patterns have matched, then the input is invalid, so there's no Command

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
 = let store_items = items store in
       case integerToFin pos (size store) of
            Nothing   => Just ("Out of range \n", store)
            (Just id) => Just (index id store_items ++ "\n", store)


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
         Nothing => Just ("Invalid command \n", store)
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item) -- returns a string that gives the position, and updates the datastore with the new data
         Just (Get pos) => getEntry pos store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

-}
