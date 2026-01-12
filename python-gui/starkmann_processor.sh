#!/bin/bash
# Unix/Linux/macOS shell script to run Starkmann Email Processor
# Make executable with: chmod +x starkmann_processor.sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"
python3 starkmann_email_processor.py "$@"
