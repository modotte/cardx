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

You'll need these to be installed:
  - GHC 9.2.5 (could be obtained via ghcup)
  - Recent `stack` or `cabal` executable (could be obtained via ghcup).
  - System dependencies (this is for Ubuntu 22.04):
    - libglew-dev
    - libsdl2-ttf-dev
    - libsdl2-image-dev
    - libsdl2-gfx-dev
 

### Running

Run `stack run` or `cabal run` from inside the project directory to launch program.

### Testing

Run `stack test` or `cabal test` to execute unit tests.

## LICENSE
This software has been released under the BSD-3-Clause license.
For more details, please see LICENSE file.
