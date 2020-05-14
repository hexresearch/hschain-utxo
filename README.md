# hschain-utxo

UTXO-based contracts for hschain.
Language for smart contracts and service to work with UTXO model.

For a description read the [docs](https://hexresearch.github.io/hschain-utxo/)


### How to build

Project is built with nix. See README.md in the nix directory
for instructions.

### How to run tests

Tests define several scenarios of funds exhabge. 
They start nodes allocate money to users and trigger exchange of funds
on various conditions. 

We should run tests in the directory hschain-utxo-test. 

```
> cd hschain-utxo-test
> cabal new-run hschain-utxo-test
```

### How to update docs

See this workflow as a reference on how to update the docs:

https://daler.github.io/sphinxdoc-test/includeme.html

General workflow to update docs:

* cd to the main repo and edit doc-files in the ```doc``` directory.
* build docs locally with ```make html```
* commit changes
* switch to the html-doc repo ```../hschain-utxo-docs```
* commit html changes and submit it to the main repo 
* ```git commit -a -m "rebuilt docs"```
* ```git push origin gh-pages```




