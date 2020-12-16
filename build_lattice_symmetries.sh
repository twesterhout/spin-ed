#!/bin/bash

set -e

TEST=0
BUILD=1
SHARED=0
while [[ $# -gt 0 ]]; do
	key="$1"
	case $key in
		--test) TEST=1; shift; ;;
		--shared) SHARED=1; shift; ;;
		--clean) BUILD=0; shift; ;;
		*)
			echo "Unknown argument: $1"
			echo "Usage: ./build_lattice_symmetries.sh [--test] [--shared] [--clean]"
			exit 1
		;;
	esac
done

export PREFIX="$PWD/third_party/lattice_symmetries"
WORKDIR="$PWD/third_party/build"

mkdir -p "$WORKDIR" && cd "$WORKDIR"
if [ ! -d "lattice-symmetries" ]; then
	[ $VERBOSE -eq 1 ] && echo "Downloading lattice-symmetries"
	git clone --depth 1 git@github.com:twesterhout/lattice-symmetries
fi
cd "lattice-symmetries"
git pull origin master
git submodule update --init --recursive

if [ $BUILD -eq 1 ]; then
	mkdir -p build && cd build

	export CXXFLAGS="-march=nocona -mtune=ivybridge"
	cmake -GNinja -DCMAKE_INSTALL_PREFIX=$PREFIX \
				  -DCMAKE_BUILD_TYPE=Debug \
				  -DBUILD_SHARED_LIBS=$SHARED \
				  -DLatticeSymmetries_ENABLE_UNIT_TESTING=$TEST \
				  ..
	cmake --build .
	cmake --build . --target install
else
	rm -rf build
	rm -rf $PREFIX/*
fi
