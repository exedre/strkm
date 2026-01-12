# User Manual for the `.books` Navigation and Management Program

This program enables efficient navigation, selection, and export of entries in a `.books` file. Below is an overview of how to use the program's key features and shortcuts.

---

### Basic Navigation and Viewing

- **Opening the `.books` File**: The program reads `.books` files and allows easy movement between entries. Entries can be browsed directly in the main window.
  
- **Viewing Entries with Mouse Clicks**: Clicking on an entry brings that specific entry into view at the top of the window.

- **Bringing Up the Entries List**: Press **`C-t`** to open a table of all entries, displaying information from each book entry in a tabular format. This table provides a quick overview for easy navigation.

---

### Navigating within the Entries Table

Once the entries list is open:

- **Move between entries**: Use the **arrow keys** to scroll through the list of entries. Each selection in the list automatically brings the corresponding entry in the `.books` file into view.

- **Selecting or Deselecting an Entry**: 
    - Press **`SPC`** on a highlighted entry to toggle its selection status (select or deselect).
    - Selected entries will be marked for easy identification.

- **Toggling Selection for All Entries**:
    - **`C-a`** inverts the selection for all entries (deselects selected entries and vice versa).
    - **`C-u C-a`** selects or deselects all entries in one go, depending on their current selection status.

---

### Exporting Selected Entries

- **Export to CSV, ORG, or XLSX format**: Press **`C-e`** to export the selected entries to a file. 
    - You will be prompted to enter a filename with either `.csv`, `.org`, or `.xlsx` extension to choose your preferred format.
    - Supported formats:
        - **CSV format**: Each entry's details will be saved in a structure separated by a customizable delimiter (default: `|`), compatible with spreadsheet applications.
        - **ORG format**: Entries are structured as an org-mode table, compatible with Emacs's Org mode, ideal for further text-based manipulation and organization.
        - **XLSX format**: Converts the exported data into a Microsoft Excel file for direct use with spreadsheet software.
    - When exporting to `.xlsx`, the program will first generate a temporary CSV file and convert it to XLSX using a Python script (see Configuration below).

---

### Configuration

To customize the program for your needs, you can set the following variables:

1. **`strkm-csv-sep`**: Defines the delimiter used in CSV files. Default is `"|"`.
   - Example:
     ```elisp
     (setq strkm-csv-sep ",") ;; Use commas as the delimiter
     ```

2. **`strkm-python-csv2xlsx-command`**: Specifies the Python interpreter to use for XLSX conversion. Default is `"python3"`.
   - Example:
     ```elisp
     (setq strkm-python-csv2xlsx-command "/usr/local/bin/python3")
     ```

3. **`strkm-python-command`**: Path to the Python script that converts CSV to XLSX. Default is `"~/bin/csv2xlsx.py"`.
   - Example:
     ```elisp
     (setq strkm-python-command "~/scripts/my-csv2xlsx.py")
     ```

   **Note**: Ensure the Python script is accessible and compatible with your system.

---

### Notes on Exporting

- Ensure the selected file extension (`.csv`, `.org`, or `.xlsx`) matches the intended format. If the extension is incorrect, the export will fail.
- For XLSX export:
  - The program uses a temporary CSV file as an intermediate step.
  - The Python script `csv2xls.py` must be correctly configured and executable.

---

This program provides a streamlined method for reading, organizing, and exporting data from a `.books` file. Whether you're preparing data for spreadsheets, further textual analysis, or presentation, the program offers flexible options to suit your workflow.