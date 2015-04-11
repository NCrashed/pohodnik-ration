# Pohodnik-Ration

Tool to calculate ration for a hike. It is local web application written in Haskell (compiled to JavaScript).

To compile the application you need a [Haste](http://haste-lang.org/) compiler.

# Quick guide for compilation

1. Install haskell platform
2. Execute:
```
cabal install haste-compiler
haste-boot
```
3. Clone this repo and execute in root folder of the repo:
```
haste-inst install --dependencies-only
haste-inst configure
haste-inst build
```
4. Open in web browser Main.html