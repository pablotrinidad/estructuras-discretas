# Práctica 06 - *¿Qué película es?*

Simple CLI game where the user needs to guess a movie's name based on
the emoji representation of it, i.e:

```
[1] Jugar 🎮
[2] Instrucciones 📜
[3] Salir ❌
1
¿Cuál es el nombre de la película? ♥️ ♥️ ♥️ ♥️ ♥️
💉 💉 🚽 🇬🇧 👨
Shaw of the death
❌
¿Cuál es el nombre de la película? ♥️ ♥️ ♥️ ♥️
👸 📓
Queen Elizabeth
❌
¿Cuál es el nombre de la película? ♥️ ♥️ ♥️
🎪 🎈 🔪 👦
IT
✅

[1] Jugar 🎮
[2] Instrucciones 📜
[3] Salir ❌
3
Ok ciao 👋
```

*Note:* Full PSet description can be found [here](http://lenguajesfc.com/Discretas-javi/ed_20191_p06.html).

## Usage

The only requirement is to have the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
and then the entry point is **`Main.hs`**, so you can compile it the way you want, I like
using `$ runhaskell Main.hs`.

## Overview

The whole game is divided in 3 modules: Main, Utils and Game, additionally the
movies data is loaded from a `.txt` file located at `data/`.

* **`Main.hs`**: Handle game execution and menu presentation
* **`Utils.hs`**: Utilities used in the Game module for manipulating movie strings
* **`Game.hs`**: Main game logic, also handle the loading of movies data from file

## Contributors

* [**Pablo Trinidad (419004279)**](https://github.com/pablotrinidad)
