# Data Files

This directory contains sample data files used for testing and examples.

## Structure

### books/
Contains .books format files (raw processed email data):
- `ELENCO DI LIBRI.books` - Sample book list

### exports/
Contains exported data in various formats:
- `.csv` - Comma-separated values
- `.org` - Org-mode format (Emacs)
- `.xlsx` - Excel spreadsheet

## File Types

### .books Files
Raw format containing parsed Starkmann email data with:
- Block separators (===...===)
- Book information organized in blocks
- Used for data preservation and archival

### .csv Files
Comma-separated values with delimiter |
Columns: Authors | Ed | Title | City | Publisher | Year | ISBN | Format | Currency | Price

### .org Files
Org-mode table format, compatible with Emacs
Can be imported into spreadsheet applications

### .xlsx Files
Excel spreadsheet format
- Headers in row 1
- Data starting from row 2
- 10 columns for bibliographic information

## Usage

- Use for testing both Emacs and Python applications
- Reference for data format specifications
- Examples for integration tests

## Size Information

- .books files: ~200-400 KB
- .csv files: ~5-10 KB
- .xlsx files: ~30-50 KB
