**WORK IN PROGRESS**

# spin-ed

[![Hackage](https://img.shields.io/hackage/v/spin-ed.svg?logo=haskell)](https://hackage.haskell.org/package/spin-ed)
[![Stackage Lts](http://stackage.org/package/spin-ed/badge/lts)](http://stackage.org/lts/package/spin-ed)
[![Stackage Nightly](http://stackage.org/package/spin-ed/badge/nightly)](http://stackage.org/nightly/package/spin-ed)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

User-friendly exact diagonalization for spin systems.


## Installing

We provide pre-built AppImages for Linux. Go to [Releases](...) page, download
the AppImage to your location of choice, and make it executable. That's it!

Or, if you prefer the command line:
```bash
wget https://github.com/twesterhout/spin-ed/...
chmod +x SpinED-x86_64.AppImage
```


### Specifying symmetries

The general syntax for specifying a symmetry in the input YAML file is the
following:

```yaml
permutation: <list-of-integers>
sector: <integer>
```

