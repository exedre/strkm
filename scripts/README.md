# Utility Scripts

This directory contains helper scripts for project maintenance and development.

## Contents

### update-version.sh
Shell script for automatic version updates in project files.

**Usage:**
```bash
./update-version.sh <new-version>
```

**What it does:**
- Updates version numbers in source files
- Updates version in documentation
- Updates version in configuration files
- Maintains consistency across project

**Example:**
```bash
./update-version.sh 1.1.0
```

## Notes

- Make sure script is executable: `chmod +x update-version.sh`
- Run from project root directory
- Creates backup of modified files

## Future Scripts

This directory can contain:
- Build scripts
- Deployment scripts
- Testing automation
- Release management scripts
