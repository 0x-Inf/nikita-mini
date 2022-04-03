module Conversation

import NikitaCore.CoreData
-- import InputOutput.Interact

%access public export


processConversationInput : (item : String) -> String
processConversationInput item
  = let words_in = words item in
        show (length words_in)
