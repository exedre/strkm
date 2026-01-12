# Emacs strkm-mode

This directory contains the original Emacs Lisp implementation of the Starkmann email processor.

## Contents

- **strkm-mode.el** - Main Emacs major mode (506 lines)
- **strkm-mode.el.bak** - Backup of mode file
- **csv2xls.py** - Python script for CSV to Excel conversion
- **dot-emacs.el** - Emacs configuration example
- **2024_11_26.books** - Sample .books data file

## Features

- Email file processing and parsing
- Book bibliographic data extraction
- CSV and ORG format export
- Excel conversion (requires external script)

## Usage

1. Load `strkm-mode.el` in Emacs
2. Use menu: Tools → Library → Process and Export XLS
3. Select email file to process
4. Choose output format (CSV, ORG, or XLSX)

## Requirements

- GNU Emacs 24.3 or higher
- Python (for XLSX export)
- pandas library (for XLSX conversion)

## Documentation

See main README.md in docs/ directory for detailed information.

## Status

✓ Fully functional
✓ Version 1.01
✓ Tested and verified
