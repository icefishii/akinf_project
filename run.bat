@echo off
REM =============================================================================
REM AkinfProject - Windows Run Script
REM =============================================================================
REM This script builds and runs the Haskell stock analysis application
REM with comprehensive test coverage and error handling.

echo ==========================================
echo    AKINF-PROJECT STOCK ANALYSIS TOOL
echo ==========================================
echo.
echo Starting build and test process...
echo.

REM Check if cabal is available
where cabal >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Cabal build tool not found!
    echo Please install GHC and Cabal from: https://www.haskell.org/ghc/
    echo.
    pause
    exit /b 1
)

REM Update package index
echo [1/5] Updating Cabal package index...
cabal update
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Failed to update package index
    pause
    exit /b 1
)
echo Package index updated successfully.
echo.

REM Build the project
echo [2/5] Building the project...
cabal build --enable-optimization
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Build failed
    pause
    exit /b 1
)
echo Build completed successfully.
echo.

REM Run comprehensive test suite
echo [3/5] Running test suite ^(29 tests^)...
cabal test --enable-optimization
if %ERRORLEVEL% NEQ 0 (
    echo WARNING: Some tests failed - but continuing with execution
    echo.
) else (
    echo All tests passed successfully!
    echo.
)

REM Run comprehensive benchmarks
echo [4/5] Running Benchmarks...
cabal bench --enable-optimization
if %ERRORLEVEL% NEQ 0 (
    echo WARNING: Some Benchmarks failed - but continuing with execution
    echo.
) else (
    echo All Benchmarks executed successfully!
    echo.
)

REM Run the application
echo [5/5] Running stock analysis application...
echo.
echo ==========================================
echo           APPLICATION OUTPUT
echo ==========================================
cabal run akinf-project --enable-optimization
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Application execution failed
    pause
    exit /b 1
)
