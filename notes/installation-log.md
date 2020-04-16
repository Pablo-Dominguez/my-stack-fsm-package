## Haskell installation



* $ sudo pacman -S ghc
* $ sudo pacman -S cabal-install 
* $ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic 

Lo anterior lanzó errores

* $ cabal update
* $ sudo pacman -S haskell-haddock-library
* $ sudo pacman -S ghc-static

Configuramos cabal y cabal-install

* $ cd
* $ mv .cabal/config .cabal/config-dep
* $ cabal user-config update


## IDE installation

https://www.vacationlabs.com/haskell/environment-setup.html
https://downloads.haskell.org/~ghc/5.04/docs/html/users_guide/ghci.html  **important!!**

## Building package

https://wiki.haskell.org/Cabal
https://cabal.readthedocs.io/en/latest/developing-packages.html#using-cabal-init

* $ cd Documentos/haskell
* $ cabal init


## Documenting package 

https://wiki.haskell.org/Haddock

## Repository 

http://hackage.haskell.org
