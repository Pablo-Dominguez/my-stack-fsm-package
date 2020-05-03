# A haskell package in model checking

This is the repo for the package I developed in haskell as the project for my dissertation. The package consists in the following three parts:

### FSM.Automata

In this module, we define the `data Automata` so it is possible to construct an Automata, perform basic operations with it or get information from it.

### FSM.States

In this module we strongly use the [Data Map](http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html) package to store information in the states. Although this module may seem simple, it took a lot of hours of designing.

I decided to keep the packages independents from each other so you can use one without having to use the other. Because of this, it is possible to create an `AutomataInfo` object of an `Automata` that is not even defined.

### FSM.Logic

In this final module, we combine the two previous modules and some functions implemented in them so we can apply the CTL model checking algorithm over the `Automata` and the `AutomataInfo`. 
