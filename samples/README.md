# Sample Files

This directory contains sample email files and working data for testing.

## Structure

### Email Files (.eml)
- `ForwardedMessage.eml` - Sample email (1.5 MB)
- `ForwardedMessage2.eml` - Additional sample email (547 KB)

These are actual Starkmann bibliographic emails used for testing and development.

### lavorazioni/ (Working/Processing Folder)
Contains email files and intermediate processing results:
- Sample Starkmann emails with different dates
- Test files for various scenarios
- Processing artifacts and results

## Usage

### For Testing
1. Copy sample email file to a working directory
2. Run Python GUI: `python starkmann_email_processor.py`
3. Select the email file
4. Process and export to Excel/CSV/ORG

### File Format
Starkmann emails contain:
- Email headers (From, To, Date, Subject)
- Formatted email body with book data
- Block separators: ================================================================(XXXX)
- Each block contains book information

### Encoding
- Original encoding: Windows-1252
- Processed encoding: UTF-8
- Handles special characters automatically

## Notes

- Files are original samples (not corrupted)
- Can be used for reproducing issues
- Useful for performance testing (especially larger files)
- Email dates are preserved in filenames for organization

## Related

See `data/` directory for processed/exported files.
