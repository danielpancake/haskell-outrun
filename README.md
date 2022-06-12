# HaskellGame


## Troubleshooting
| Problem | Solution |
| - | - |
|  On Windows `stack run` yields `user error (unknown GLUT entry glutInit)` | Download [freeglut package](https://www.transmissionzero.co.uk/software/freeglut-devel/), extract archive, and copy `freeglut.dll` to the Windows folder. When executable is being distributed, distribute it alongside with `freeglut.dll` |