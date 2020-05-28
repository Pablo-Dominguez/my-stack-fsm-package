import qualified Data.Matrix as M

## Create the automata
mat = M.fromLists [[2,3],[1,3],[0,3]]
tom = createAutomata 3 ['a', 'b'] 1 mat [3]

## Add the state information

info_state1 = fromlsStateInfo 1 [("atom1", Atom "p"),("atom2", Atom "q")] Nothing
info_state2 = fromlsStateInfo 2 [("atom1", Atom "q"),("atom2", Atom "r")] (Just info_state1)
info_state3 = fromlsStateInfo 3 [("atom1", Atom "r")] (Just info_state2)

## Models

* modelsCTL (Or (Atom "p") (Atom "q")) tom info_state3

-- output: True


* modelsCTL (Not (Atom "r")) tom info_state3

-- output: True

* modelsCTL (CTrue) tom info_state3

-- output: True

* modelsCTL (EX (And (Atom "q") (Atom "r"))) tom info_state3 --descubrí varios errores en el caso EX
-- en alguno de los siguientes se tiene q ^ r

-- output: True

* modelsCTL (Not (AX (And (Atom "q") (Atom "r")))) tom info_state3
-- no siempre en los siguientes se tiene que q ^ r 

-- output: True

* modelsCTL (Not (EF (And (Atom "p") (Atom "r")))) tom info_state3
-- en ningún estado futuro se tiene p ^ r 

-- output: True

* modelsCTL (EG (Atom "r")) (changeInitialState tom 3) info_state3

-- output: True

* modelsCTL (AF (Atom "r")) tom info_state3
-- En todos los caminos, en algún punto se tiene.

-- output: True

* modelsCTL (EU (And (Atom "p") (Atom "q")) (Atom "r")) tom info_state3
-- existe un camino donde se tiene p ^ q hasta que se tiene r

-- output: True

* modelsCTL (AU (Atom "p") (Atom "r")) tom info_state3
-- en todos los caminos se tiene p hasta que se tiene r

--output: True

* modelsCTL (AG (RArrow (Or (Or (Atom "p") (Atom "q")) (Atom "r")) (EF (EG (Atom "r"))))) tom info_state3
-- En todos los caminos se tiene siempre que si se tiene p v q v r entonces a partir de ese momento en algún punto se va a tener que siempre r

-- output: True
