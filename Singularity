Bootstrap: library
From: twesterhout/default/alpine-openblas:latest
Stage: build_openblas

Bootstrap: docker
From: utdemir/ghc-musl:v17-ghc884
Stage: base

%files from build_openblas
    /usr/lib/libopenblas*          /usr/lib/
    /usr/include/cblas.h           /usr/include/
    /usr/include/f77blas.h         /usr/include/
    /usr/include/lapack.h          /usr/include/
    /usr/include/openblas_config.h /usr/include/
    /usr/lib/pkgconfig/blas.pc     /usr/lib/pkgconfig/blas.pc
    /usr/lib/pkgconfig/lapack.pc   /usr/lib/pkgconfig/lapack.pc

%post
    set -eu
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal
    BUILD=0

    apk update
    apk add --no-cache \
        build-base gcc g++ gfortran cmake linux-headers python3 \
        git pkgconfig hdf5-dev hdf5-static
    cabal v2-update
    # Creating .pc files for hdf5 and hdf5_hl
    ln -s /usr/lib/pkgconfig/hdf5-1.12.0.pc /usr/lib/pkgconfig/hdf5.pc
    ln -s /usr/lib/pkgconfig/hdf5_hl-1.12.0.pc /usr/lib/pkgconfig/hdf5_hl.pc

    mkdir -p /project/spin-ed.local
    if [ $BUILD -eq 1 ]; then
      cd /project
      git clone --depth=1 "https://github.com/twesterhout/spin-ed.git"
      cd spin-ed/
      cabal v2-build --enable-executable-static
    fi

%environment
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal
    export TERM=xterm-256color
