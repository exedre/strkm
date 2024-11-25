#!/bin/bash

# Nome del file Emacs Lisp da aggiornare
FILE="strkm/strkm-mode.el"

# Ottieni l'ultimo tag di Git
VERSION=$(git describe --tags --abbrev=0)

# Aggiorna il file con la nuova versione
if [[ -n "$VERSION" ]]; then
  sed -i.bak -E "s/(defconst strkm-version \")[^\"]*(\")/\1$VERSION\2/" "$FILE"
  echo "Updated version in $FILE to $VERSION"
else
  echo "No Git tag found. Version not updated."
  exit 1
fi
