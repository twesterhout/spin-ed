# SpinED

[![GitHub CI](https://github.com/twesterhout/spin-ed/workflows/CI/badge.svg)](https://github.com/twesterhout/spin-ed/actions)
[![GitHub Release](https://img.shields.io/github/v/release/twesterhout/spin-ed?include_prereleases)](https://github.com/twesterhout/spin-ed/releases)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

User-friendly exact diagonalization package for quantum many-body systems.


## 🔧 Installing

We provide pre-built [AppImage](https://appimage.org/)s for Linux. Go to
[Releases](https://github.com/twesterhout/spin-ed/releases) page, download the
AppImage to your location of choice, and make it executable. That's it! 🥳

Or, if you prefer the command line:
```sh
wget https://github.com/twesterhout/spin-ed/releases/download/continuous/SpinED-x86_64.AppImage
chmod +x SpinED-x86_64.AppImage
```


## 📝 Usage

Using `SpinED` is quite simple. After installing the AppImage just feed it your
input yaml file, and that's it. For example:

```sh
./SpinED-x86_64.AppImage my_system.yml
```

where `my_system.yml` looks like this:

```yaml
basis:
  number_spins: 4
  symmetries: []
hamiltonian:
  name: "Heisenberg Hamiltonian"
  terms:
    - matrix: [[1,  0,  0,  0],
               [0, -1,  2,  0],
               [0,  2, -1,  0],
               [0,  0,  0,  1]]
      sites: [[0, 1], [1, 2], [2, 3], [3, 0]]
observables: []
```

This will create a file `exact_diagonalization_result.h5` which contains the
ground state of the Hamiltonian.

```console
$ h5dump -H exact_diagonalization_result.h5
HDF5 "exact_diagonalization_result.h5" {
GROUP "/" {
   GROUP "basis" {
      DATASET "representatives" {
         DATATYPE  H5T_STD_U64LE
         DATASPACE  SIMPLE { ( 16 ) / ( 16 ) }
      }
   }
   GROUP "hamiltonian" {
      DATASET "eigenvalues" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      }
      DATASET "eigenvectors" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SIMPLE { ( 16, 1 ) / ( 16, 1 ) }
      }
      DATASET "residuals" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      }
   }
}
}
```

And we can check that it computed the correct energy:
```console
$ h5dump -d /hamiltonian/eigenvalues exact_diagonalization_result.h5
HDF5 "exact_diagonalization_result.h5" {
DATASET "/hamiltonian/eigenvalues" {
   DATATYPE  H5T_IEEE_F64LE
   DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
   DATA {
   (0): -8
   }
}
}
```
