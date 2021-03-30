Bootstrap: library
From: twesterhout/default/alpine-openblas:sha256.4812ceb0d1fe9c270dae93a24ab3eae506bf27ebc91985f8325bc8b5c9e18ba8
Stage: build_openblas

Bootstrap: docker
From: utdemir/ghc-musl:v17-ghc8104
Stage: base

%files from build_openblas
    /usr/lib/libopenblas*          /usr/lib/
    /usr/include/cblas.h           /usr/include/
    /usr/include/f77blas.h         /usr/include/
    /usr/include/lapack.h          /usr/include/
    /usr/include/openblas_config.h /usr/include/
    /usr/lib/pkgconfig/blas*       /usr/lib/pkgconfig/
    /usr/lib/pkgconfig/lapack*     /usr/lib/pkgconfig/

%post
    set -eu
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal
    BUILD=1

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
      cabal v2-build \
        --enable-executable-static \
        --enable-executable-stripping \
        --enable-optimization=2
      cp -v $(find dist-newstyle/build -name "spin-ed" -type f) "SpinED-$(git rev-parse --short HEAD)"
    fi

%environment
    export LC_ALL=C.UTF-8
    export CABAL_DIR=/usr/local/.cabal
    export TERM=xterm-256color
