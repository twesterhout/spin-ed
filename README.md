# SpinED

[![GitHub CI](https://github.com/twesterhout/spin-ed/workflows/CI/badge.svg)](https://github.com/twesterhout/spin-ed/actions)
[![GitHub Release](https://img.shields.io/github/v/release/twesterhout/spin-ed?include_prereleases)](https://github.com/twesterhout/spin-ed/releases)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

User-friendly exact diagonalization package for quantum many-body systems.


## Installing

We provide pre-built AppImages for Linux. Go to [Releases](...) page, download
the AppImage to your location of choice, and make it executable. And that's it!

Or, if you prefer the command line:
```sh
wget https://github.com/twesterhout/spin-ed/releases/download/continuous/SpinED-x86_64.AppImage
chmod +x SpinED-x86_64.AppImage
```


## Usage


### Specifying symmetries

The general syntax for specifying a symmetry in the input YAML file is the
following:

```yaml
permutation: <list-of-integers>
sector: <integer>
```

