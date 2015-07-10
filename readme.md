#Dude, where is my unit file?

###Description

OpenSUSE features some packages which lack a systemd unit file. By using the
Arch Linux package repository this program will try to figure out which packages
are affected.

###Compilation

Needs librpm: `pkg-config --exists rpm`, `ghc`, `cabal` and the haskell packages
from the `.cabal` file. Hint: Use `cabal sandbox`!

Just type `make`. A binary should be ready in
`dist/build/DudeWhereIsMyUnitFile`.

###Usage

**Warning**: Expect a runtime of around 3 to 4 hours!

`arg[1]`: OBS login
`arg[2]`: OBS password
`arg[3]`: (optional) path to file where output should be written.
                     If not provided, stdout is used.

Hint: `read $user; read $pass; DudeWhereIsMyUnitFile $user $pass ..`
