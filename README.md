# User Manual for the `.books` Navigation and Management Program

This program enables efficient navigation, selection, and export of entries in a `.books` file. Below is an overview of how to use the program's key features and shortcuts.

### Basic Navigation and Viewing

- **Opening the `.books` File**: The program reads `.books` files and allows easy movement between entries. Entries can be browsed directly in the main window.
  
- **Viewing Entries with Mouse Clicks**: Clicking on an entry brings that specific entry into view at the top of the window.

- **Bringing Up the Entries List**: Press **`C-t`** to open a table of all entries, displaying information from each book entry in a tabular format. This table provides a quick overview for easy navigation.

### Navigating within the Entries Table

Once the entries list is open:

- **Move between entries**: Use the **arrow keys** to scroll through the list of entries. Each selection in the list automatically brings the corresponding entry in the `.books` file into view.

- **Selecting or Deselecting an Entry**: 
    - Press **`SPC`** on a highlighted entry to toggle its selection status (select or deselect).
    - Selected entries will be marked for easy identification.

- **Toggling Selection for All Entries**:
    - **`C-a`** inverts the selection for all entries (deselects selected entries and vice versa).
    - **`C-u C-a`** selects or deselects all entries in one go, depending on their current selection status.

### Exporting Selected Entries

- **Export to CSV or ORG format**: Press **`C-e`** to export the selected entries to a file. 
    - You will be prompted to enter a filename with either `.csv` or `.org` extension to choose your preferred format.
    - In **CSV format**, each entry's details will be saved in a comma-separated structure, compatible with spreadsheet applications.
    - In **ORG format**, entries are structured as an org-mode table, compatible with Emacs's Org mode, and ideal for further text-based manipulation and organization.

**Note**: Ensure you enter the correct extension in the filename prompt, as only `.csv` and `.org` are supported.

This program thus provides a streamlined method for reading, organizing, and exporting data from a `.books` file, enhancing productivity with convenient navigation and flexible selection options.