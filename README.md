# cardx

Simple GUI based, custom Uno card game (two players) made to learn Haskell 9 and Monomer.

![image](https://user-images.githubusercontent.com/85026096/215523966-74348942-3bca-4dbb-a9f0-3a3434cc19a1.png)


## Custom rules
- No need to signal *Uno* when the player left with one card.
- Standard penalties doesn't exist.
- Prohibit players to stack any cards except Skip card. (apparently official rule?)
- Player can pull any amount of card they want from deck until they found 
available matching card. 
- There's no pick one card from deck and move on action.
- Note: These rules might be changed in the future.

## Building

You'll need GHC 9.2.5 and recent stack (or Cabal) executable installed.

Run `stack run` in the project directory to build and launch the game.

If the build failed and complained about missing external dependencies, you
might need SDL2 libraries (i.e `SDL2-devel`) to get installed first. See
[Monomer](https://github.com/fjvallarino/monomer) for more unlisted GUI dependencies

## LICENSE
This software has been released under the BSD-3-Clause license.
For more details, please see LICENSE file.