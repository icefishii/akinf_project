AKINF-PROJECT - HASKELL STOCK ANALYSIS TOOL
=============================================

QUICK START GUIDE
==================

This is a comprehensive Haskell stock analysis application that provides 
advanced financial analytics including ROI calculations, trend detection, 
moving averages, risk assessment, and portfolio correlation analysis.

REQUIREMENTS:
- Haskell GHC 9.6.7+ and Cabal build tool
- Windows, Linux, or macOS

COMPILATION & EXECUTION:
1. Open terminal/command prompt in project directory
2. Run: cabal update
3. Run: cabal build  
4. Run: cabal test     (optional - runs 29 unit tests)
5. Run: cabal run akinf-project

QUICK RUN SCRIPTS:
- Windows: Double-click run.bat or run from cmd
- Linux/macOS: chmod +x run.sh && ./run.sh

CONFIGURATION:
Edit config.yaml to specify stocks and date range:
  stocks: ["AAPL", "MSFT", "GILD", "ADP"]
  timeframe:
    start_date: "2013-09-24"
    end_date: "2018-01-16"

FEATURES:
- Best/worst day analysis for each stock
- ROI calculation and trend detection (Upward/Downward/Sideways)
- Risk assessment based on volatility (Low/Medium/High)
- Moving averages (SMA 20/50/200) with trading signals
- Portfolio correlation matrix and diversification analysis
- Comprehensive summary statistics

For detailed information, see README.md