# Starkmann Email to Excel Processor GUI

A professional **Python GUI application** for Windows, macOS, and Linux that converts Starkmann email files containing bibliographic book data into Excel format.

## 📋 Overview

This application provides a simple, user-friendly graphical interface to:
- Select email files with book bibliographic data
- Automatically extract and parse book information
- Export data directly to Excel (XLSX) format
- Create accompanying `.books` files for data preservation

Compatible with **Python 3.7.2** and newer, with native support for Windows GUI.

## ✨ Features

✅ **GUI Interface** - Point-and-click file selection and processing  
✅ **Email Processing** - Automatically parses Starkmann email format  
✅ **Excel Export** - Generates professional XLSX files with formatted data  
✅ **Auto Date Extraction** - Detects dates in filenames (YYYY_MM_DD or DD-MMM-YY)  
✅ **Books File Generation** - Creates `.books` file alongside Excel export  
✅ **Error Handling** - Clear error messages and success notifications  
✅ **Python 3.7+ Compatible** - Works with Python 3.7.2 through 3.12+  

## 🖥️ System Requirements

### Windows
- Python 3.7+ ([download](https://www.python.org/downloads/))
  - Check "Add Python to PATH" during installation
  - tkinter is included by default

### macOS
- Python 3.7+ (`brew install python3`)
- May need tkinter separately

### Linux (Ubuntu/Debian)
- Python 3.7+
- `sudo apt-get install python3-tk`

## 📦 Quick Start (Windows)

1. Install Python 3.7+ from [python.org](https://www.python.org/downloads/)
   - Check "Add Python to PATH"

2. Download strkm-gui folder

3. Open command prompt in the folder:
   ```bash
   pip install -r requirements.txt
   python starkmann_email_processor.py
   ```

4. A window will open - select your email file and click "Process & Export"

## 🚀 Usage

### GUI Mode

1. Run: `python starkmann_email_processor.py` or double-click `starkmann_processor.bat`

2. **Input File**: Click Browse to select email file (.txt, .eml)

3. **Output File**: Auto-populated based on date in filename, modify if needed

4. **Process & Export**: Click to process and generate:
   - `<date>.xlsx` - Excel spreadsheet with 10 columns
   - `<date>.books` - Raw data file

### Column Headers in Excel

| Authors | Ed | Title | City | Publisher | Year | ISBN | Format | Currency | Price |
|---------|-----|-------|------|-----------|------|------|--------|----------|-------|

## 🔍 Supported Date Formats

- `2024_11_25.txt` → 2024_11_25
- `25-Nov-24.txt` → 2024_11_25
- Current date (if no date detected)

## 🛠️ Troubleshooting

| Error | Solution |
|-------|----------|
| "Python not found" | Reinstall Python with "Add to PATH" checked |
| "No tkinter" | Windows: Reinstall Python. Linux: `apt install python3-tk` |
| "No openpyxl" | Run: `pip install openpyxl` |
| File encoding error | Resave file as UTF-8 |

## 📁 Files Included

```
strkm-gui/
├── starkmann_email_processor.py    Main application
├── test_processor.py               Tests
├── requirements.txt                Dependencies
├── starkmann_processor.bat         Windows launcher
├── starkmann_processor.sh          Linux launcher
└── README.md                       This file
```

## 🧪 Testing

```bash
python test_processor.py
```

## 📋 Dependencies

- **openpyxl** ≥2.6.0 (Excel export)
- tkinter (included with Python)

## 📄 License

GNU General Public License v3.0

## 👤 Author

Emmanuele Somma <emmanuele@exedre.org>

---

**Version**: 1.0.0  
**Python**: 3.7.2+
