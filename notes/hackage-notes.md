
```shell
> $ cd my-fsm-stack-package
> $ stack sdist (create distribution package and check it)
> $ stack upload
```

## Upload package docs

```shell
> $ cp -r `stack path --dist-dir`/doc/html/PACKAGE PACKAGE-VERSION-docs
> $ cp -r .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/doc/html/FSM docs/FSM-0.0.3.0-docs
> $ 
> $ tar -c -v -z --format=ustar -f PACKAGE-VERSION-docs.tar.gz PACKAGE-VERSION-docs
> $ tar -c -v -z --format=ustar -f FSM-0.0.3.0-docs.tar.gz FSM-0.0.3.0-docs 
> $ curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@PACKAGE-VERSION-docs.tar.gz" "https://USERNAME:PASSWORD@hackage.haskell.org/package/PACKAGE-VERSION/candidate/docs"
```
