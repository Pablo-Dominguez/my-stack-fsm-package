
> $ cd my-fsm-stack-package
> $ stack sdist (create distribution package and check it)
> $ stack upload

## Upload package docs
```
cp -r `stack path --dist-dir`/doc/html/PACKAGE PACKAGE-VERSION-docs
tar -c -v -z --format=ustar -f PACKAGE-VERSION-docs.tar.gz PACKAGE-VERSION-docs
curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@PACKAGE-VERSION-docs.tar.gz" "https://USERNAME:PASSWORD@hackage.haskell.org/package/PACKAGE-VERSION/candidate/docs"
```
