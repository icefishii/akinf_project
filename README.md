# AkinfProject - Advanced Stock Analysis Tool

A comprehensive Haskell-based stock market analysis application that provides advanced financial analytics including trend detection, risk assessment, moving averages, and portfolio correlation analysis.

## Features

### 📈 **Stock Analytics**
- **Best/Worst Day Analysis**: Find the highest gain and loss days for each stock
- **Return on Investment (ROI)**: Calculate total ROI over the specified timeframe
- **Trend Detection**: Classify trends as Upward, Downward, or Sideways
- **Risk Assessment**: Evaluate volatility and assign risk levels (Low/Medium/High)

### 📊 **Technical Indicators**
- **Simple Moving Averages**: SMA 20, 50, and 200-day calculations
- **Trading Signals**: Bullish, Bearish, and Neutral signals based on MA crossovers
- **Volatility Analysis**: Daily volatility calculations and averaging

### 🔗 **Portfolio Analysis**
- **Correlation Matrix**: Calculate correlations between all stock pairs
- **Diversification Assessment**: Evaluate portfolio risk spread
- **Multi-Stock Support**: Analyze multiple stocks simultaneously

## Requirements

### System Requirements
- **Haskell**: GHC 9.6.7 or later
- **Cabal**: Latest version (comes with GHC)
- **Operating System**: Windows, Linux, or macOS

### Haskell Dependencies
All dependencies are automatically managed by Cabal:
- `vector` - Efficient arrays
- `yaml` - YAML configuration parsing
- `cassava` - CSV file parsing
- `bytestring` - Byte string operations
- `containers` - Map and Set data structures
- `tasty` - Testing framework
- `tasty-hunit` - Unit testing

## Installation

### 1. Clone or Download the Project
```bash
# If using git
git clone <repository-url>
cd akinf-project

# Or download and extract the project files
```

### 2. Install Haskell (if not already installed)

#### Windows:
- Download GHC from: https://www.haskell.org/ghc/download.html
- Or use Chocolatey: `choco install ghc cabal`

#### Linux (Ubuntu/Debian):
```bash
sudo apt-get update
sudo apt-get install ghc cabal-install
```

#### macOS:
```bash
# Using Homebrew
brew install ghc cabal-install
```

### 3. Update Cabal Package Index
```bash
cabal update
```

## Project Structure

```
akinf-project/
├── app/
│   └── Main.hs                 # Application entry point
├── src/
│   └── AkinfProject/
│       ├── Calculate.hs        # Core analytics calculations
│       ├── Config.hs          # Configuration parsing
│       ├── CSV.hs             # CSV data parsing
│       ├── Filter.hs          # Data filtering
│       └── Output.hs          # Result formatting and display
├── test/
│   ├── Spec.hs               # Comprehensive test suite (29 tests)
│   └── test-config.yaml      # Test configuration
├── config.yaml               # Main configuration file
├── all_stocks_5yr.csv        # Sample stock data
├── akinf-project.cabal       # Project dependencies and build config
├── README.md                 # This file
├── run.bat                   # Windows run script
└── run.sh                    # Linux/macOS run script
```

## Configuration

Edit `config.yaml` to specify which stocks to analyze and the timeframe:

```yaml
stocks: ["AAPL", "MSFT", "GILD", "ADP"]  # Stock symbols to analyze
timeframe:
  start_date: "2013-09-24"               # Start date (YYYY-MM-DD)
  end_date: "2018-01-16"                 # End date (YYYY-MM-DD)
```

## Usage

### Building the Project
```bash
# Build the project
cabal build

# Build and run in one command
cabal run akinf-project
```

### Running Tests
```bash
# Run the complete test suite (29 tests)
cabal test

# Run tests with verbose output
cabal test --test-show-details=always
```

### Quick Start Scripts

#### Windows:
```cmd
# Double-click or run from command prompt
run.bat
```

#### Linux/macOS:
```bash
# Make executable and run
chmod +x run.sh
./run.sh
```

### Manual Execution
```bash
# Step-by-step manual execution
cabal update              # Update package index
cabal build              # Build the project
cabal test               # Run tests
cabal run akinf-project  # Run the application
```

## Sample Output

```
==========================================
         STOCK ANALYSIS RESULTS
==========================================
Stock: AAPL
  ----------
  Best day:  2015-08-24 (8.70% gain)
  Worst day: 2015-08-25 (-6.63% loss)
  Total ROI: 152.16%
  Trend:     Upward
  Volatility: 1.64% (daily avg)
  Risk Level: Low (Conservative investment)
  Moving Averages:
    SMA 20:  $70.20
    SMA 50:  $73.24
    SMA 200: $79.24
    Signal:  Bearish (Strong sell signal)

==========================================
SUMMARY:
  Total stocks analyzed: 4
  Stocks with valid data: 4
  Highest single-day gain: 11.72%
  Average ROI: 103.99%
  Average volatility: 1.71%
  Risk distribution: Low: 4, Medium: 0, High: 0

==========================================
       PORTFOLIO CORRELATION ANALYSIS
==========================================
Stock Correlations:
  AAPL <-> MSFT: 0.415 (Moderate)
  ADP <-> MSFT: 0.421 (Moderate)
Average Correlation: 0.324
Portfolio Diversification: Moderately Diversified
==========================================
```

## Data Format

The application expects CSV data with the following columns:
- `date` - Date in YYYY-MM-DD format
- `open` - Opening price
- `high` - Highest price of the day
- `low` - Lowest price of the day
- `close` - Closing price
- `volume` - Trading volume
- `Name` - Stock symbol

## Technical Details

### Architecture
- **Modular Design**: Separated concerns across multiple modules
- **Type Safety**: Leverages Haskell's strong type system
- **Functional Programming**: Pure functions with clear data flow
- **Error Handling**: Robust handling of missing/invalid data

### Algorithms
- **ROI Calculation**: `(final_price - initial_price) / initial_price * 100`
- **Volatility**: Daily `(high - low) / open * 100`, then averaged
- **Correlation**: Pearson correlation coefficient between daily returns
- **Moving Averages**: Simple moving averages over specified periods
- **Risk Assessment**: Based on volatility thresholds (Low < 4%, Medium < 8%, High >= 8%)

### Performance
- **Efficient Data Structures**: Uses Vector for numerical computations
- **Lazy Evaluation**: Haskell's lazy evaluation optimizes memory usage
- **Streaming**: Processes large CSV files efficiently

## Testing

The project includes comprehensive test coverage:
- **29 Unit Tests** covering all calculation functions
- **Edge Case Testing** for missing data and boundary conditions
- **Integration Tests** for complete workflow validation
- **Mathematical Accuracy** verification for all financial calculations

Run tests with:
```bash
cabal test
```

## Troubleshooting

### Common Issues

1. **"Command not found: cabal"**
   - Install GHC and Cabal tools
   - Ensure they're in your system PATH

2. **Build errors**
   - Run `cabal update` to refresh package index
   - Check GHC version compatibility

3. **CSV parsing errors**
   - Verify CSV file format matches expected headers
   - Check for encoding issues (should be UTF-8)

4. **Empty results**
   - Verify stock symbols exist in the CSV data
   - Check date range in config.yaml

### Debug Mode
For detailed error information, run with verbose output:
```bash
cabal run akinf-project --verbose
```

## Development

### Adding New Indicators
1. Add calculation function to `src/AkinfProject/Calculate.hs`
2. Update `StockAnalysis` data type if needed
3. Add display logic to `src/AkinfProject/Output.hs`
4. Write unit tests in `test/Spec.hs`

### Code Style
- Follow standard Haskell conventions
- Use type signatures for all top-level functions
- Include Haddock documentation comments
- Run tests before committing changes

## License

This project is available under the terms specified in the LICENSE file.

## Support

For issues or questions:
1. Check the troubleshooting section above
2. Review the test suite for usage examples
3. Examine the source code - it's well-documented
