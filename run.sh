#!/bin/bash
# =============================================================================
# AkinfProject - Linux/macOS Run Script
# =============================================================================
# This script builds and runs the Haskell stock analysis application
# with comprehensive test coverage and error handling.

set -e  # Exit on any error

echo "=========================================="
echo "   AKINF-PROJECT STOCK ANALYSIS TOOL"
echo "=========================================="
echo
echo "Starting build and test process..."
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}$1${NC}"
}

print_success() {
    echo -e "${GREEN}$1${NC}"
}

print_warning() {
    echo -e "${YELLOW}$1${NC}"
}

print_error() {
    echo -e "${RED}$1${NC}"
}

# Check if cabal is available
if ! command -v cabal &> /dev/null; then
    print_error "ERROR: Cabal build tool not found!"
    echo "Please install GHC and Cabal:"
    echo "  Ubuntu/Debian: sudo apt-get install ghc cabal-install"
    echo "  macOS: brew install ghc cabal-install"
    echo "  Or visit: https://www.haskell.org/ghc/"
    exit 1
fi

# Check if we're in the right directory
if [ ! -f "akinf-project.cabal" ]; then
    print_error "ERROR: akinf-project.cabal not found!"
    echo "Please run this script from the project root directory."
    exit 1
fi

# Update package index
print_status "[1/4] Updating Cabal package index..."
if cabal update; then
    print_success "Package index updated successfully."
else
    print_error "ERROR: Failed to update package index"
    exit 1
fi
echo

# Build the project
print_status "[2/4] Building the project..."
if cabal build --enable-optimization; then
    print_success "Build completed successfully."
else
    print_error "ERROR: Build failed"
    exit 1
fi
echo

# Run comprehensive test suite
print_status "[3/4] Running test suite (29 tests)..."
if cabal test --enable-optimization; then
    print_success "All tests passed successfully!"
else
    print_warning "WARNING: Some tests failed - but continuing with execution"
fi
echo

# Run the application
print_status "[4/4] Running stock analysis application..."
echo
echo "=========================================="
echo "          APPLICATION OUTPUT"
echo "=========================================="

if cabal run akinf-project --enable-optimization; then
    echo
    echo "=========================================="
    echo "           EXECUTION COMPLETE"
    echo "=========================================="
    echo
    print_success "The stock analysis has completed successfully."
    echo "Check the output above for detailed analytics including:"
    echo "  - Best/worst performing days for each stock"
    echo "  - ROI calculations and trend analysis"
    echo "  - Risk assessment and volatility metrics"
    echo "  - Moving averages and trading signals"
    echo "  - Portfolio correlation analysis"
    echo
    echo "To run again: ./run.sh or use 'cabal run akinf-project'"
    echo "To run tests only: use 'cabal test'"
    echo
else
    print_error "ERROR: Application execution failed"
    exit 1
fi
