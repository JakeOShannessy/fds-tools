set windows-powershell := true
alias b := build

_default:
    @just --list

# Run the tests
test:
    cargo test

# Build the debug binaries
build:
    cargo b

# Build the release binaries
build-release:
    cargo b --release

# Build release and create MSI package
package: build-release
    mkdir -Force dist
    candle AffCalcsInstaller.wxs
    light AffCalcsInstaller.wixobj

# Clean the ./dist folder
clean-dist:
    rm -r dist

# Clean everything
clean: clean-dist
    cargo clean
