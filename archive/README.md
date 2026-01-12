# Archive

This directory contains backup files, temporary files, and old versions.

## Contents

### Backup Files (*~)
Files created automatically during editing:
- `ELENCO DI LIBRI.books~` - Backup of books file
- `ou.org~` - Backup of org export
- `out.csv~` - Backup of csv export
- `out.org~` - Backup of org export
- Various `.el~` files - Emacs mode backups

### Temporary Files (#...#)
Emacs lock/temporary files:
- `#ELENCO DI LIBRI.books#` - Temporary file
- `#dot-emacs.el#` - Temporary emacs config

### Configuration Files
Old configuration files:
- `.emacs` - Old Emacs configuration
- `.emacs~` - Backup of old configuration

### Old Code
- `sterk-it.el~` - Old elisp code

## Purpose

Archive contains files that:
- Are no longer actively used
- Are backups of current versions
- Are temporary/lock files
- Should be preserved for reference

## Cleanup

Temporary files (#...#) can generally be safely deleted.
Backup files (*~) can be deleted if no longer needed.
Configuration files should be archived if replaced.

## Notes

- These files were moved here to keep root directory clean
- Can be referenced if needed for version history
- Consider using version control (git) instead of manual backups
