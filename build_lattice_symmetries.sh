#!/bin/bash

set -e

SHARED=0
BUILD=1
while [[ $# -gt 0 ]]; do
	key="$1"
	case $key in
	--shared)
		SHARED=1
		shift
		;;
	--clean)
		BUILD=0
		shift
		;;
	*)
		echo "Unknown argument: '$1'"
		echo "Usage: build_lattice_symmetries.sh [--shared] [--clean]"
		exit 1
		;;
	esac
done

export PREFIX="$PWD/third_party/lattice_symmetries"
WORKDIR="$PWD/third_party/build"

mkdir -p "$WORKDIR" && cd "$WORKDIR"
if [ ! -d "lattice-symmetries" ]; then
	echo "Downloading lattice-symmetries..."
	git clone --depth 1 https://github.com/twesterhout/lattice-symmetries.git
fi
cd "lattice-symmetries"
git pull origin master
git submodule update --init --recursive

if [ $BUILD -eq 1 ]; then
	mkdir -p build && cd build

	export CXXFLAGS="-fvisibility-inlines-hidden -march=nocona -mtune=haswell -fstack-protector-strong -fno-plt -ffunction-sections"
	export LDFLAGS="-Wl,--as-needed -Wl,--gc-sections -Wl,-z,now -Wl,--disable-new-dtags"
	cmake -GNinja -DCMAKE_INSTALL_PREFIX=$PREFIX \
		-DCMAKE_BUILD_TYPE=Release \
		-DBUILD_SHARED_LIBS=$SHARED \
		-DLatticeSymmetries_ENABLE_UNIT_TESTING=OFF \
		..
	cmake --build .
	cmake --build . --target install
else
	rm -rf build/
	rm -rf $PREFIX/*
fi
