import qualified Data.Matrix as M

## Create the automata
mat = M.fromLists [[2,3],[1,3],[0,3]]
tom = createAutomata 3 ['a', 'b'] 1 mat [3]

## Add the state information

info_state1 = fromlsStateInfo 1 [("atom1", Atom "p"),("atom2", Atom "q")] Nothing
info_state2 = fromlsStateInfo 2 [("atom1", Atom "q"),("atom2", Atom "r")] (Just info_state1)
info_state3 = fromlsStateInfo 3 [("atom1", Atom "r")] (Just info_state2)

## Check CTL

* checkCTL (Or (Atom "p") (Atom "q")) tom info_state3

-- output: fromList [(1,True),(2,True),(3,False)]

* checkCTL (Not (Atom "r")) tom info_state3

-- output: fromList [(1,True),(2,False),(3,False)]

* checkCTL (EX (And (Atom "q") (Atom "r"))) tom info_state3 --descubrí varios errores en el caso EX
-- en alguno de los siguientes se tiene q ^ r

-- output: fromList [(1,True),(2,False),(3,False)]

* checkCTL (Not (AX (And (Atom "q") (Atom "r")))) tom info_state3
-- no siempre en los siguientes se tiene que q ^ r 

-- output: fromList [(1,True),(2,True),(3,True)]

* checkCTL (Not (EF (And (Atom "p") (Atom "r")))) tom info_state3
-- en ningún estado futuro se tiene p ^ r 

-- output: fromList [(1,True),(2,True),(3,True)]

* checkCTL (EG (Atom "r")) tom info_state3
-- existe un camino donde siempre se tiene r

-- output: fromList [(1,False),(2,True),(3,True)]

* checkCTL (AF (Atom "r")) tom info_state3
-- En todos los caminos, en algún punto se tiene.

-- fromList [(1,True),(2,True),(3,True)]

* checkCTL (EU (And (Atom "p") (Atom "q")) (Atom "r")) tom info_state3
-- existe un camino donde se tiene p ^ q hasta que se tiene r

-- output: fromList [(1,True),(2,True),(3,True)]

* checkCTL (AU (Atom "p") (Atom "r")) tom info_state3
-- en todos los caminos se tiene p hasta que se tiene r
--output: fromList [(1,True),(2,True),(3,True)]

* checkCTL (AG (RArrow (Or (Or (Atom "p") (Atom "q")) (Atom "r")) (EF (EG (Atom "r"))))) tom info_state3
-- En todos los caminos se tiene siempre que si se tiene p v q v r entonces a partir de ese momento en algún punto se va a tener que siempre r

-- output: fromList [(1,True),(2,True),(3,True)]


