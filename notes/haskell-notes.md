## Haskell notes

* $ ghci

vamos a hacer una prueba de importar algo que no esté instalado y luego volver a importarlo después de instalarlo para ver si en ghci carga

```
Prelude> import Control.Concurrent.AlarmClock

<no location info>: error:
    Could not find module ‘Control.Concurrent.AlarmClock’
    It is not a module in the current program, or in any known package.
```

* $ cabal install --ghc-option=-dynamic alarmclock
* $ ghci
```
Prelude> import Control.Concurrent.AlarmClock
Prelude Control.Concurrent.AlarmClock> 
```
works!
