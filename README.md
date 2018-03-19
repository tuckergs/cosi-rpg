# cosi-rpg
The adventures of an old, decrepit college student named Reece.

## What?
This is a non-violent RPG written in Haskell about my pal Reece who is on the quest to get a degree. He will have to face many obstacles, like anthropomorphic homeworks, smart professors, and James. 

This is written in Haskell, and it will (probably) always be written in Haskell. However, adding content shouldn't cause monadic brain damage.

This is also a work in progress; I have a demo and the battle API right now.

## How to compile

For people who are more experienced in the ways of the Haskell, you just need Cabal packages random and mtl, which you probably already have

If you do not know the way, get Haskell Platform and then run these commands

```
cabal install random
cabal install mtl
```

To run my demo, you then need to run

```
ghc BattleDemo.hs
```



