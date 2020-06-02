# Relación 5

import qualified Data.Matrix as M

## Ejercicio 4

mat = M.fromLists [[1,2,4],[3,0,0],[2,0,0],[3,0,0]]
tom = createAutomata 4 ['a', 'b','c'] 1 mat [1,3]

info_state1 = fromlsStateInfo 1 [("atom1", Atom "r")] Nothing
info_state2 = fromlsStateInfo 2 [("atom1", Atom "p"),("atom2", Atom "t"), ("atom3", Atom "r")] (Just info_state1)
info_state3 = fromlsStateInfo 3 [("atom1", Atom "q"),("atom2", Atom "r")] (Just info_state2)
info_state4 = fromlsStateInfo 4 [("atom1", Atom "p"),("atom2", Atom "q")] (Just info_state3)

### Con estado inicial s0

* modelsCTL (RArrow (Not (Atom "p")) (Atom "r")) tom info_state4

-- output: True

* modelsCTL (AF (Atom "t")) tom info_state4

-- output: False

* modelsCTL (Not (EG (Atom "r"))) tom info_state4

-- output: False

* modelsCTL (EU (Atom "t") (Atom "q")) tom info_state4

-- output: False

* modelsCTL (AF (Atom "q")) tom info_state4

-- output: True

* modelsCTL (EF (Atom "q")) tom info_state4

-- output: True

* modelsCTL (EG (Atom "r")) tom info_state4

-- output: True

* modelsCTL (EG (Or (Atom "r") (Atom "q"))) tom info_state4

-- output: True

### Con estado inicial s2

* modelsCTL (RArrow (Not (Atom "p")) (Atom "r")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (AF (Atom "t")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (Not (EG (Atom "r"))) (changeInitialState tom 2) info_state4

-- output: False

* modelsCTL (EU (Atom "t") (Atom "q")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (AF (Atom "q")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (EF (Atom "q")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (EG (Atom "r")) (changeInitialState tom 2) info_state4

-- output: True

* modelsCTL (EG (Or (Atom "r") (Atom "q"))) (changeInitialState tom 2) info_state4

-- output: True

formulas = [(RArrow (Not (Atom "p")) (Atom "r")),(AF (Atom "t")),(Not (EG (Atom "r"))), (EU (Atom "t") (Atom "q")), (AF (Atom "q")),(EF (Atom "q")), (EG (Atom "r")), (EG (Or (Atom "r") (Atom "q")))]

## Ejercicio 5

mat = M.fromLists [[2,4],[2,3],[1,4],[1,0]]
tom = createAutomata 4 ['a', 'b'] 1 mat [1,3]

info_state1 = fromlsStateInfo 1 [("atom1", Atom "p"),("atom2", Atom "q")] Nothing
info_state2 = fromlsStateInfo 2 [("atom1", Atom "r")] (Just info_state1)
info_state3 = fromlsStateInfo 3 [("atom1", Atom "p"),("atom2", Atom "t")] (Just info_state2)
info_state4 = fromlsStateInfo 4 [("atom1", Atom "q"),("atom2", Atom "r")] (Just info_state3)

### Con estado inicial s0

* modelsCTL (AF (Atom "t")) tom info_state4

-- False

* modelsCTL (AG (EF (Or (Atom "p") (Atom "r")))) tom info_state4

-- True

* modelsCTL (EX (EX (Atom "r"))) tom info_state4

-- True

* modelsCTL (AG (AF (Atom "q"))) tom info_state4

-- False

### Con estado inicial s2

* modelsCTL (AF (Atom "t")) (changeInitialState tom 2) info_state4

-- False

* modelsCTL (AG (EF (Or (Atom "p") (Atom "r")))) (changeInitialState tom 2) info_state4

-- True

* modelsCTL (EX (EX (Atom "r"))) (changeInitialState tom 2) info_state4

-- True

* modelsCTL (AG (AF (Atom "q"))) (changeInitialState tom 2) info_state4

-- False


formulas = [(AF (Atom "q")),(AG (EF (Or (Atom "p") (Atom "r")))),(EX (EX (Atom "r"))),(AG (AF (Atom "q")))]





######################

mat = M.fromLists [[2,3,0],[3,2,0],[3,2,4],[2,5,0],[5,0,0]]
tom1 = createAutomata 5 ['a', 'b','c'] 1 mat []
tom2 = createAutomata 5 ['a', 'b','c'] 2 mat []
tom3 = createAutomata 5 ['a', 'b','c'] 3 mat []
tom4 = createAutomata 5 ['a', 'b','c'] 4 mat []
tom5 = createAutomata 5 ['a', 'b','c'] 5 mat []
--Creamos la informacion de los estados
info_state1 = fromlsStateInfo 1 [] Nothing
info_state2 = fromlsStateInfo 2 [("atom1", Atom "p"),("atom2", Atom "q")] (Just info_state1)
info_state3 = fromlsStateInfo 3 [("atom1", Atom "p"),("atom2", Atom "q")] (Just info_state2)
info_state4 = fromlsStateInfo 4 [("atom1", Atom "p"),("atom2", Atom "r")] (Just info_state3)
info_state5 = fromlsStateInfo 5 [("atom1", Atom "p")] (Just info_state4)
--creamos la lista de formulas

formulas = [EG (Atom "p"),AG (Atom "p"), EF (AG (Atom "p")),AU (Atom "p") (EG (RArrow (Atom "p") (Atom "q"))), EU (Or (And (Atom "p") (Atom "q")) (Atom "r")) (EU (Atom "r") (AG (Atom "p")))]

checkFormulas tom1 info_state5 formulas []
checkFormulas tom2 info_state5 formulas []
checkFormulas tom3 info_state5 formulas []
checkFormulas tom4 info_state5 formulas []
checkFormulas tom5 info_state5 formulas []

