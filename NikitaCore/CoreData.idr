module CoreData

import Data.Vect

%access public export

record Graph where
       constructor MkGraph
       node_size : Nat
       edge_size : Nat
       nodes : Vect node_size elem
       edges : Vect edge_size (elem, elem)

data Tree a = Empty | Node (Tree a) (Tree a)

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | SS | T | U | V | W | X | Y | Z

Word : (size : Nat) -> Type
Word nl = Vect nl Letter

-- Sentence : (size : Nat) -> Type
-- Sentence nw = ?what (Vect nw Word)

record CoreState where
       constructor MkNikitaCoreState
       size : Nat
       current : Vect n String
       state_rep : Vect size (Vect n String)
