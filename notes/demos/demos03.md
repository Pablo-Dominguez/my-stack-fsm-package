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
