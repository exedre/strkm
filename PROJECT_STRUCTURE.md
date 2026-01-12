# Starkmann Project Structure

This document describes the organized directory structure of the project.

## Project Overview

**Starkmann Email to Excel Processor** - Tools for processing Starkmann bibliographic email files into structured data formats.

Available in two implementations:
- **Emacs strkm-mode** - Elisp implementation for GNU Emacs
- **Python GUI** - Standalone Python application with GUI

## Directory Structure

```
2024.Starkmann/
│
├── 📘 docs/                          Documentation & Guides
│   ├── INDEX.md
│   ├── README.md                     Main project documentation
│   └── PYTHON_GUI_PROJECT_CREATED.txt
│
├── 💻 emacs-mode/                    Emacs Lisp Implementation
│   ├── README.md
│   ├── strkm-mode.el                 Main Emacs mode (506 lines)
│   ├── strkm-mode.el.bak             Backup
│   ├── csv2xls.py                    CSV to Excel converter
│   ├── dot-emacs.el                  Configuration example
│   ├── 2024_11_26.books              Sample data file
│   └── [various backups & temps]
│
├── 🐍 python-gui/                    Python 3.7+ GUI Application
│   ├── README.md
│   ├── starkmann_email_processor.py  Main application (506 lines)
│   ├── test_processor.py             Test suite (223 lines)
│   ├── setup.py                      Installation script
│   ├── requirements.txt              Dependencies (openpyxl)
│   ├── starkmann_processor.bat       Windows launcher
│   ├── starkmann_processor.sh        Unix launcher
│   ├── INSTALL.txt                   Installation guide
│   ├── README_PYAPP.md               User documentation
│   ├── TEST_REPORT.txt               Test results
│   ├── PROJECT_SUMMARY.txt           Technical specs
│   └── venv/                         Virtual environment
│
├── 📊 data/                          Sample Data Files
│   ├── README.md
│   ├── books/
│   │   └── *.books                   Raw .books format data
│   └── exports/
│       ├── *.csv                     Exported CSV files
│       ├── *.org                     Exported Org-mode files
│       └── *.xlsx                    Exported Excel files
│
├── 📧 samples/                       Sample Email & Working Files
│   ├── README.md
│   ├── *.eml                         Sample Starkmann emails
│   └── lavorazioni/                  Working/processing folder
│
├── 📹 media/                         Video & Media Files
│   ├── README.md
│   └── *.mp4                         Screencasts & demonstrations
│
├── 🔧 scripts/                       Utility Scripts
│   ├── README.md
│   └── *.sh                          Shell scripts
│
├── 🗑️ archive/                       Backups & Old Files
│   ├── README.md
│   ├── *~                            Backup files
│   ├── #*#                           Temporary files
│   └── [old versions]
│
├── .git/                             Git repository
├── .gitignore                        Git ignore rules
└── [root config files]
```

## Quick Navigation

### For Users
1. **Python GUI Users**: See `python-gui/README.md` and `python-gui/INSTALL.txt`
2. **Emacs Users**: See `emacs-mode/README.md`
3. **General Info**: See `docs/README.md`

### For Developers
1. **Code**: See `python-gui/` and `emacs-mode/` directories
2. **Tests**: See `python-gui/test_processor.py`
3. **Documentation**: See `python-gui/PROJECT_SUMMARY.txt`
4. **Test Results**: See `python-gui/TEST_REPORT.txt`

### For Data
1. **Sample Data**: See `data/books/` and `data/exports/`
2. **Sample Emails**: See `samples/`
3. **Working Files**: See `samples/lavorazioni/`

## Directory Purpose Summary

| Directory | Purpose | Contains |
|-----------|---------|----------|
| **docs/** | Project documentation | READMEs, guides, overviews |
| **emacs-mode/** | Emacs implementation | .el files, elisp code, config |
| **python-gui/** | Python application | Python source, GUI, tests |
| **data/** | Sample & test data | .books, .csv, .xlsx files |
| **samples/** | Example inputs | Email files for testing |
| **media/** | Videos & media | Screencasts, demos |
| **scripts/** | Utilities | Build, update, deploy scripts |
| **archive/** | Backups & old files | Obsolete versions, temps |

## File Organization Rules

- ✓ Source code in appropriate language folders
- ✓ Documentation in `docs/`
- ✓ Test data in `data/`
- ✓ Sample inputs in `samples/`
- ✓ Backups and old versions in `archive/`
- ✓ Scripts in `scripts/`
- ✓ Media in `media/`

## Getting Started

1. **Choose Implementation**
   - Python GUI: Better for most users (easy to install, no dependencies)
   - Emacs mode: For Emacs users who want integration

2. **Read Documentation**
   - Start with: `python-gui/README.md` or `emacs-mode/README.md`
   - Installation: `python-gui/INSTALL.txt` or `emacs-mode/README.md`

3. **Try It Out**
   - Use sample files from `samples/`
   - Check test results in `python-gui/TEST_REPORT.txt`

## Clean Structure Benefits

✓ Easy to find files
✓ Clear separation of concerns
✓ Scalable organization
✓ Professional appearance
✓ Easier maintenance
✓ Better for collaboration

## Maintenance

- Keep `archive/` clean (remove obsolete backups periodically)
- Update README files when adding new content
- Use git for version control, not backup files
- Keep structure consistent with new additions

---

**Project Version**: 1.0.0
**Last Updated**: January 9, 2026
**Status**: Production Ready
