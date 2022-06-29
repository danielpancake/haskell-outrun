# haskell-outrun

Do not edit `haskell-outrun.cabal` by hand as it is being generated automatically from `package.yaml`


## How to run
**Option 1**: Run `stack run` in the console. After resolving dependencies, executable should run.

**Option 2**: In VScode press `F6` and choose `haskell run` option.


## Troubleshooting
| Problem | Solution |
| - | - |
| If it does not compile, first try | `stack clean` -> `stack build` -> `stack run` |
|  On Windows `stack run` yields `user error (unknown GLUT entry glutInit)` | Download [freeglut package](https://www.transmissionzero.co.uk/software/freeglut-devel/), extract archive, and copy `freeglut.dll` to the Windows folder. When executable is being distributed, distribute it alongside with `freeglut.dll` |