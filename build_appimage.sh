#!/bin/bash

set -e
set -o pipefail

SCRIPT_DIR="$(
	cd "$(dirname "$0")"
	pwd -P
)"
PROJECT_DIR="$(
	cd "${SCRIPT_DIR}"
	pwd -P
)"
LINUXDEPLOY=
APP=

print_help() {
	echo ""
	echo "Usage: ./build_appimage.sh [--help]"
	echo ""
	echo "This script builds and packages SpinED in an AppImage."
	echo ""
	echo "Options:"
	echo "  --help         Display this message."
	echo ""
}

install_linuxdeploy() {
	[[ -n $LINUXDEPLOY ]] && return 0
	if ! which linuxdeploy; then
		echo "Installing linuxdeploy into ${PROJECT_DIR}/third_party..."
		LINUXDEPLOY="${PROJECT_DIR}/third_party/linuxdeploy-x86_64.AppImage"
		if [[ ! -f "$LINUXDEPLOY" ]]; then
			wget -O "$LINUXDEPLOY" \
				"https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage"
			chmod +x "$LINUXDEPLOY"
		else
			echo "Already installed"
		fi
	else
		LINUXDEPLOY=$(which linuxdeploy)
	fi
}

build_app() {
	[[ $# -ne 1 ]] && {
		echo "$0 expects a single argument: installdir"
		return 1
	}
	declare -r installdir="$1"
	# Set GHC version
	# ghcup set ghc 8.8.4
	# ghc --version
	# Build the package
	cd "$PROJECT_DIR"
	cabal v2-install exe:spin-ed \
		--disable-tests --disable-benchmarks \
		--installdir="$installdir" --install-method=copy
	APP="$installdir/spin-ed"
}

create_appimage() {
	install_linuxdeploy
	# Create destination directory
	cd "$PROJECT_DIR"
	if [[ -d AppDir ]]; then
		echo "Removing 'AppDir'..."
		rm -r AppDir/
	fi
	mkdir -p AppDir/usr/bin
	build_app "AppDir/usr/bin"
	declare -r version=$("$APP" --version)
	VERSION=$version "$LINUXDEPLOY" \
		--appdir="AppDir" \
		--executable="$APP" \
		--desktop-file="resources/SpinED.desktop" \
		--icon-file="resources/SpinED.png" \
		--output appimage
	mv -v SpinED-$version-x86_64.AppImage SpinED-x86_64.AppImage
	rm -r AppDir/
}

while [[ $# -gt 0 ]]; do
	key="$1"
	case $key in
	--help)
		print_help
		exit 0
		;;
	*)
		echo "Unknown argument: '$1'"
		echo ""
		echo "Try using '--help' to get an overview of supported options."
		exit 1
		;;
	esac
done
create_appimage
