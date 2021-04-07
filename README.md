# SpinED

[![GitHub CI](https://github.com/twesterhout/spin-ed/workflows/CI/badge.svg)](https://github.com/twesterhout/spin-ed/actions)
[![GitHub Release](https://img.shields.io/github/v/release/twesterhout/spin-ed?include_prereleases)](https://github.com/twesterhout/spin-ed/releases)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

User-friendly exact diagonalization package for quantum many-body systems.


## :wrench: Installing

We provide pre-built static executables for Linux. Go to
[Releases](https://github.com/twesterhout/spin-ed/releases) page and download the
executable to your location of choice. That's it! :partying_face:

> :information_source: **Note:** executables are currently tagged by git
> commits from which they were built. It is suggested that after downloading
> the application you create a symbolic link to it:
>
> ~~~~~~~sh
> ln --symbolic SpinED SpinED-8b0138b # the commit hash may differ in your case
> ~~~~~~~



## 📝 Usage

Using `SpinED` is quite simple. Just feed it your input [yaml
file](https://en.wikipedia.org/wiki/YAML). For example:

```sh
./SpinED my_system.yaml
```

where `my_system.yaml` looks like this:

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

This will create a file `exact_diagonalization_result.h5` which will contain
the ground state of the Hamiltonian.

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

This was a very simple example! Have a look at [`template.yaml`](./template.yaml)
which describes all supported fields. [`example/`](./example/) folder also
contains various usage examples.


## Contributing and support

If you use this package for your research and have questions or suggestions,
please, don't hesitate to contact me on Github or
[email](https://www.ru.nl/tcm/about-us/phd-students/westerhout/).

Also, if the fact that most code here is written in Haskell doesn't scare you,
feel free to create a pull request implementing new features or fixing bugs!

