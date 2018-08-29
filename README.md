# Raycaster

This project is for fun to learn about raycasting graphics and test out the 
Haskell SDL2 bindings.  

The code written is based off of the write-up found 
[here](https://lodev.org/cgtutor/raycasting.html).  


## Building

Running `cabal install --only-dependencies` and `cabal build` should provide 
a working version of the demo executable in a *dist* directory.  For docs, 
run `cabal haddock --executable` to generate html files to browse.

I highly recommend installing in a sandbox (`cabal sandbox init`) first so 
your system installations are not bloated and do not clash with the 
dependencies for this project.  I do not know the equivalent commands using 
stack, but I would happily take a pull-request or email with the instructions 
using that tool.
