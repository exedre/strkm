;;; strkm-books-mode.el --- A major mode for managing .books files -*- lexical-binding: t; -*-

;; Author: Emmanuele Somma <emmanuele@exedre.org>
;; Maintainer: Emmanuele Somma <emmanuele@exedre.org>
;; Created: 2024-11-06
;; Version: v1.0
;; Keywords: data, books, convenience
;; URL: https://github.com/exedre/strkm
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `strkm-books-mode` is a major mode for managing book data in .books files.
;; It provides a table-like view, parsing, and exporting capabilities.

;;; Code:

(require 'tabulated-list)


(defconst strkm-version "v1.0"
  "Current version of `strkm-books-mode`.")


(defgroup strkm nil
  "Customization group for strkm."
  :group 'applications)


(defcustom strkm-csv-sep "|"
  "Separator for CSV output."
  :type 'string
  :group 'strkm)


(defcustom strkm-print nil
  "If non-nil, print output."
  :type 'boolean
  :group 'strkm)


(defcustom strkm-debug t
  "Enable debugging."
  :type 'boolean
  :group 'strkm)

(defcustom strkm-default-package-url "http://example.com/strkm/strkm-mode.el"
  "Default URL for loading or reloading the `strkm` package.")

(defcustom strkm-python-command "~/.virtualenvs/emacsenv/bin/python3"
  "Interprete Python.
Questo comando deve essere configurato correttamente e disponibile nel percorso.")


(defcustom strkm-python-csv2xls-command "~/bin/csv2xls.py"
  "Comando per trasformare un file CSV in formato XLS usando un interprete Python.
Questo comando deve essere configurato correttamente e disponibile nel percorso.")

(defcustom strkm-books-columns
  '(("Authors" 20 t)
    ("Ed" 5 t)
    ("Title" 30 t)
    ("City" 15 t)
    ("Publisher" 20 t)
    ("Year" 5 t)
    ("ISBN" 15 t)
    ("Format" 10 t)
    ("Currency" 5 t)
    ("Price" 10 t))
  "List of columns to display in the table, with their widths and sorting options.
Each element in the list is a list of three values: column name, width, and sorting."
  :group 'strkm
  :type '(repeat (list (string :tag "Column Name")
		       (integer :tag "Column Width")
		       (boolean :tag "Sortable"))))

(defvar strkm-source-file nil)

(defvar strkm-selected-rows nil
  "List of selected rows in the tabulated list.")

(defvar strkm-books-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-t") 'strkm-books-display-tabulated)  ;; Toggle between views
    (define-key map (kbd "C-q") 'kill-this-buffer)
    (define-key map (kbd "<down>") 'strkm-books-next)  
    (define-key map (kbd "<up>") 'strkm-books-previous)
    map)
  "Keymap for `strkm-books-mode'.")

(defvar strkm-books-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)  
    (define-key map [mouse-1] 'strkm-books-tabulated-click-action)  
    (define-key map (kbd "n") 'strkm-books-next-row)  
    (define-key map (kbd "p") 'strkm-books-previous-row)
    (define-key map (kbd "<down>") 'strkm-books-next-row)  
    (define-key map (kbd "<up>") 'strkm-books-previous-row)
    (define-key map (kbd "SPC") 'strkm-books-toggle-select)  ;; Toggle selection with 'SPC'
    (define-key map (kbd "C-a") 'strkm-books-toggle-select-all)  ;; 
    (define-key map (kbd "C-e") 'strkm-books-export) ;; Export to CSV with 'C-c C-e'
    (define-key map (kbd "<next>") 
      (lambda () 
        (interactive)
        (strkm-books-scroll-and-click 'scroll-up-command)))
    ;; Control-Page Up
    (define-key map (kbd "<prior>") 
      (lambda () 
        (interactive)
        (strkm-books-scroll-and-click 'scroll-down-command)))
    map)
  "Keymap for `strkm-books-tabulated-mode'.")

(defun strkm-books-scroll-and-click (scroll-fn)
  "Scroll the buffer with SCROLL-FN and perform `strkm-books-click-action`."
  (funcall scroll-fn)  ;; Esegui il comando di scroll
  ;; Esegui l'azione associata al click sul blocco corrente
  (let ((current-point (point)))
    (strkm-books-tabulated-click-action `(mouse-1 (,(selected-window) ,current-point)))))

(define-key strkm-books-mode-map [mouse-1] 'strkm-books-click-action)
(define-key strkm-books-mode-map [drag-mouse-1] 'strkm-books-click-action)
(define-key strkm-books-tabulated-mode-map [mouse-1]    'strkm-books-tabulated-click-action)



(defun strkm-books-display-tabulated ()
  "Display the contents of the .books file in a spreadsheet-like table format using tabulated-list-mode."
  (interactive)
  (delete-other-windows)
  ;; Save buffer-local variables for the window and file name
  (setq strkm-source-file buffer-file-name)
  (let ((data (strkm-parse-file buffer-file-name)))
    (with-current-buffer (get-buffer-create "*Books Table View*")
      (let ((inhibit-read-only t))       
	;; Configure tabulated-list mode
	(erase-buffer)

        ;; Dynamic configuration of columns
        (setq tabulated-list-format	      
	      (vconcat (list (list "ID" 5 t)
                             (list ">" 3 nil))
		       (mapcar (lambda (col)
				 (list (nth 0 col)  ;; Column name
				       (nth 1 col)  ;; Column width
				       (nth 2 col)))  ;; Sorting option
			       strkm-books-columns)))
	
	
	(setq tabulated-list-padding 2)
	(setq tabulated-list-entries
              (mapcar (lambda (entry)
			(let* ((key (car entry))
                               (value (cdr entry))
                               (authors (nth 0 value))
                               (ed (nth 2 value))
                               (title (nth 1 value))
                               (city (nth 3 value))
                               (publisher (nth 4 value))
                               (year (nth 5 value))
                               (isbn (nth 6 value))
                               (format (nth 7 value))
                               (currency (nth 8 value))
                               (price (nth 9 value))
			       (sel ""))
                          ;; Return each row as a list: ID and data in columns
                          (list key (vconcat
				     (list key sel)
                                     (mapcar (lambda (col)
                                               (pcase (nth 0 col)
                                                 ("Authors" authors)
                                                 ("Ed" ed)
                                                 ("Title" title)
                                                 ("City" city)
                                                 ("Publisher" publisher)
                                                 ("Year" year)
                                                 ("ISBN" isbn)
                                                 ("Format" format)
                                                 ("Currency" currency)
                                                 ("Price" price)))
                                             strkm-books-columns)))))			  
                      (strkm-hash-table-to-list data)))
	;; Use tabulated-list-mode to display the table
	(tabulated-list-init-header)
	(tabulated-list-mode)
	;; Definisci una mappa personale per i tasti
	
        (use-local-map strkm-books-tabulated-mode-map)  ;; Attiva la mappa dei tasti locale
	
	;; Imposta l'ordinamento iniziale per chiave crescente
        (tabulated-list-sort 0)	
	(tabulated-list-print)
	(split-window (selected-window) nil 'below)
	(display-buffer (current-buffer))	
        (other-window 1)
        (switch-to-buffer (current-buffer))))))

(defun strkm-tabulated-list-get-current-row ()
  "Perform an action on the current row of the tabulated list."
  (interactive)
  (tabulated-list-get-id))
    
(defun strkm-books-next-row ()
  "Go to the next row in the tabulated list and jump to the corresponding entry in the .books file."
  (interactive)
  (when (strkm-tabulated-list-get-current-row)
    (next-line)
    (strkm-books-jump-to-current)))

(defun strkm-books-previous-row ()
  "Go to the previous row in the tabulated list and jump to the corresponding entry in the .books file."
  (interactive)
  (when (strkm-tabulated-list-get-current-row)
    (previous-line)
    (strkm-books-jump-to-current)))

(defun strkm-books-jump ()
  "Jump to the corresponding entry in the .books file for the current row in the tabulated list."
  (interactive)
  (when (strkm-tabulated-list-get-current-row)
    (strkm-books-jump-to-current)))
    

(defun strkm-books-jump-to-current ()
  "Jump to the corresponding entry in the .books file for the current row in the tabulated list."
  (let* ((entry (tabulated-list-get-entry))
         (id (aref entry 0))  ;; Assume the ID is the first element in the entry vector
         (source-file strkm-source-file))
    ;; Check if there's only one window; if so, split the window and open the source file in the new one
    (if (one-window-p)
        (progn
          (split-window-below)
          (find-file source-file))  ;; Open the .books file in the new window
      (other-window 1))  ;; Switch to the other window if it already exists
    ;; Go to the beginning of the .books file
    (goto-char (point-min))
    ;; Highlight the block of text corresponding to the current entry
    (strkm-highlight-text-block id)
    ;; Reposition to the start and search for the block associated with the current entry's ID
    (goto-char (point-min))
    (search-forward (format "================================================================(%s)" id) nil t)
    ;; Center the line at the top of the window for better visibility
    (recenter-top-bottom 0)
    ;; Return focus to the original window
    (other-window 1)))

(defun strkm-hash-table-to-list (hash-table)
  "Convert a hash table to an alist."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))

(defun strkm-books-next ()
  "Move the cursor to the next entry in the buffer and highlight the current block."
  (interactive)
  (let ((current (point)))  ;; Store the current cursor position
    (if (re-search-forward "^=+\\s-*(\\([0-9]+\\))" nil t)
        (progn
          (goto-char (match-beginning 0))  ;; Move to the start of the found entry
          (forward-line)
          (strkm-highlight-current-block))  ;; Highlight the current block
      (goto-char current)  ;; Return to original position if no next entry is found
      (message "No next entry found."))))


(defun strkm-books-previous ()
  "Move the cursor to the previous entry in the buffer and highlight the current block."
  (interactive)
  (let ((current (point)))  ;; Store the current cursor position
    (if (re-search-backward "^=+\\s-*(\\([0-9]+\\))" nil t)
	(if (re-search-backward "^=+\\s-*(\\([0-9]+\\))" nil t)
            (progn
              (goto-char (match-beginning 0))  ;; Move to the start of the found entry
              (forward-line)
              (strkm-highlight-current-block))  ;; Highlight the current block
	  (goto-char current)  ;; Return to original position if no previous entry is found
	  (message "No previous entry found."))
      (message "No previous entry found"))))


(defun kill-this-buffer ()
  "Kill this buffer and close its window if there are other windows."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (and (not (window-minibuffer-p (selected-window))) ;; Non è il minibuffer
             (> (length (window-list)) 1))                ;; Ci sono altre finestre aperte
        (progn
          (kill-buffer buffer)
          (delete-window))
      (kill-buffer buffer)  ;; Solo uccidi il buffer se non puoi chiudere la finestra
      (message "Buffer killed, but window not deleted."))))


(defun strkm-books-tabulated-click-action (event)
  "Handle mouse click on a row in the tabulated list view in book-view mode."
  (interactive "e")
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      ;; Perform action, e.g., jump to corresponding entry in .books file
      (strkm-books-jump-to-current)
      (message "You clicked on the row with ID: %s" (aref entry 0)))))

(defun strkm-books-click-action (event)
  "Handle mouse click in book-view mode, moving to the clicked position and highlighting."
  (interactive "e")
  ;; Ensure that the mouse event points to a valid position
  (let* ((posn (event-start event))
         (buffer (window-buffer (posn-window posn)))
         (position (posn-point posn)))
    (with-current-buffer buffer
      (goto-char position)
      ;; Highlight the current block and retrieve the ID
      (let ((id (strkm-highlight-current-block)))
        (when id
          (message "You clicked on the block with ID: %s" id))))))


(defun strkm-highlight-current-block ()
  "Highlight the block of text where the cursor is currently located in the .books file.
The block is made bold, and any previous highlights are removed."
  (interactive)
  (let ((inhibit-read-only t))  ;; Allows modifications in the buffer
    (save-excursion
      ;; Locate the start of the block containing the cursor
      (let* ((start (re-search-backward "^=+(\\([0-9]+\\))" nil t))
             (id (when start (match-string 1))))  ;; Get the ID
        ;; If block found, pass ID to highlight function
        (when start	  
	  (goto-char (point-min))
          (strkm-highlight-text-block id))
        id))))


(defun strkm-highlight-text-block (number)
  "Highlight the text block defined by delimiters containing NUMBER.
The block will be made bold, and any previous highlights are removed."
  (interactive "sEnter the block number to highlight: ")
  
  ;; Remove previous highlights and allow modifications
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(face nil))

    ;; Locate and highlight the specified block
    (goto-char (point-min))
    (let ((start (search-forward (format "================================================================(%s)" number) nil t))
          (end (search-forward "================================================================" nil t)))
      (if (and start end)
	  (progn 
	    (goto-char start)
	    (recenter-top-bottom 0)
	    (forward-line -1)  ;; Torna alla fine della riga
            (let ((block-start (point))  ;; Move to start of line
		  (block-end (progn (goto-char end) (forward-line -1) (point))))  ;; Adjust end
              ;; Apply bold face to block
              (add-text-properties block-start block-end '(face (:weight bold)))
	      (goto-char start)
              (recenter-top-bottom 0)
              (message "Highlighted block with number %s." number)))
	(message "Block with number %s not found." number)))
  ;; Restore read-only mode
    (read-only-mode 1)))

(easy-menu-define strkm-books-menu strkm-books-mode-map
  "Menu for `strkm-books-mode`."
  '("Books"  ;; Menu name
    ["Display Table View" strkm-books-display-tabulated t]  ;; View Tabulated List
    "---"  ;; Separator
    ["Version: 0.9" nil nil]  ;; Non-interactive menu item for the version
    "---"  ;; Separator
    ["Quit" strkm-dont-prompt-save-books t]))  ;; Close the buffer


;;;###autoload
(define-derived-mode strkm-books-mode text-mode "Books"
  "Major mode for editing and viewing .books files."
  :syntax-table nil
  :abbrev-table nil
  (read-only-mode 1)
  (easy-menu-add strkm-books-menu)  ;; Aggiungi il menu personalizzato
  (use-local-map strkm-books-mode-map))

(defun strkm-x-string-trim (str)
  (if str
      str
    "-"))

(defun strkm-extract-title-authors-or-editors (line)
  "Extract the title and authors (or editors) from the LINE.
If the line contains '/ ed. by', it assumes that the part before that
is the title and the part after it (until '0.-') is the authors or editors.
Otherwise, it assumes the first part before '|' is the author and the
rest of the line is the title, replacing '|' with spaces."
  (let (title author (ed "-"))
    (if (string-match
	 "/[| ]+\\(ed\\.[| ]+\\)?by[| ]+\\(.*?\\)[| ]+0\\.-|$" line)
        ;; Case where '/ ed. by' is present
        (progn
	  (setq ed "x")
          (setq title (replace-regexp-in-string "|" " " (substring line 0 (match-beginning 0))))
	  (setq author  (replace-regexp-in-string "\\.$" ""
						  (replace-regexp-in-string "|" " " (match-string 2 line)))))
      ;; Case where '/ ed. by' is not present
      (if (string-match "^\\(.*?\\)|\\(.*\\)[| ]+0\\.-" line)
          (progn
	    (setq ed "-")
            (setq author (replace-regexp-in-string "|" " " (match-string 1 line)))
            (setq title (replace-regexp-in-string "|" " " (match-string 2 line))))
	(if (string-match "^\\(.*?\\)[| ]+0\\.-" line)
            (progn
	      (setq ed "-")
              (setq author "<Anonymous>")
              (setq title (replace-regexp-in-string "|" " " (match-string 1 line))))
	
	)))
    ;; Return the results as a list
    (list (strkm-x-string-trim title) (strkm-x-string-trim author) ed)))

(defun strkm-extract-isbn-format (line)
  "Extract a valid ISBN (10 or 13 digits) from the LINE after the last '|'."
  (let ((isbn "-") (fmt "-") (price "-") (currency "-"))
    ;; Extract potential ISBN (after the last '|' and before a space)
    (when (string-match "|\\([0-9Xx-]+\\) \\(\\(Paper\\|Hard\\)back\\) +\\[P R I C E\\] \\([A-Z]+\\)[ \t]+\\([0-9]+\\.[0-9]+\\)" line)
      (setq isbn (match-string 1 line))
      (setq fmt (match-string 2 line))
      (setq currency (match-string 4 line))
      (setq price (match-string 5 line)))
    (list isbn fmt currency price)))


(defun strkm-extract-book-details (line)
  "Extract where, publisher, year, ISBN, and price from the LINE.
  The format of the line is expected to follow this structure:
  - where: first part before ':'
  - publisher: between ':' and the first comma ','
  - year: between the first comma ',' and the first semicolon ';'
  - ISBN: after the last '|' (the ISBN number is recognized by its format)
  - prezzo: after the string '[P R I C E]'"
  (let (where publisher year isbn price info fmt currency)
    ;; Extract 'where' (before the first ':')
    (setq where (or (car (split-string line ":")) ""))
    
    ;; Extract 'publisher' (between the first ':' and the first ',')
    (if (string-match ": \\(.*?\\)," line)
        (setq publisher (or (match-string 1 line) "")))
    
    ;; Extract 'year' (between the first ',' and the first ';')
    (if (string-match ", \\([0-9]+\\)\\.-" line)
        (setq year (or (match-string 1 line) "")))
    
    ;; Extract 'ISBN' (after the last '|', assume it has a specific format)
    (setq info (strkm-extract-isbn-format line))
    (setq isbn (nth 0 info))
    (setq fmt (nth 1 info))
    (setq currency (nth 2 info))
    (setq price (nth 3 info))
    
    ;; Return the extracted details as a list
    (list (strkm-x-string-trim where)
          (strkm-x-string-trim publisher)
          (strkm-x-string-trim year)
          (strkm-x-string-trim isbn)
          (strkm-x-string-trim fmt)
          (strkm-x-string-trim currency)
          (strkm-x-string-trim price))))
      
(defun strkm-remove-pattern-from-start (line)
  "Remove the pattern '===============================================================(0001)' or similar from the beginning of LINE, where the number in parentheses can vary."
  (if (string-match "^=+([0-9]+)|" line)
      (replace-match "" nil nil line)
    line))

(defun strkm-save-info-in-blocks (block-pre block-info block-content)
  (let* ((result (strkm-extract-title-authors-or-editors
		  (strkm-remove-pattern-from-start
		   (strkm-x-string-trim block-pre))))
	 (ed (nth 2 result))
	 (author (nth 1 result))
	 (title (nth 0 result))
	 (info (strkm-extract-book-details (strkm-x-string-trim block-info)))
	 (publisher (nth 0 info))
	 (where (nth 1 info))
	 (year (nth 2 info))
	 (ISBN (nth 3 info))
	 (fmt (nth 4 info))
	 (currency (nth 5 info))
	 (price (nth 6 info)))
    (list author title ed publisher where year ISBN fmt currency price)))

;; Helper functions for extraction
(defun strkm-split-buffer-into-blocks ()
  "Splits the buffer content into blocks based on lines starting with '================================================================(XXXX)'.
   Stores each block in a hash table with the block number as the key."
  (interactive)
  (let ((blocks (make-hash-table :test 'equal))  ;; Create hash table for blocks
        (block-number nil)  ;; Key (e.g., 0002)
        (block-content nil)
	(block-pre nil)
	(block-info nil)
	(pre 0)) 
    ;; Iterate through the buffer
    (goto-char (point-min))
    (while (not (looking-at "^=+\\((\\([0-9]+\\))\\)$"))  ;; Matches the pattern ====...=======(XXXX)
      (forward-line 1))
    (looking-at "^=+\\((\\([0-9]+\\))\\)$")
    (setq block-number (match-string 2))
    (while (not (eobp))
      ;; Check if the line matches the pattern for a new block
      (cond
       ((and (eq pre 0) (looking-at "^.*0.-$"))
	(progn
	  (setq pre 1)	  
	  (setq block-pre (concat block-pre
				  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)) "|"))))
       ((and (eq pre 1) (looking-at "^ $"))
	(progn
	  (setq pre 2)
	  (setq block-info (concat block-info (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)) " "))))	  
       ((and (eq pre 2) (looking-at "^=+\\((\\([0-9]+\\))\\)$"))  ;; Matches the pattern ====...=======(XXXX)
        (progn
          ;; If we found a new block delimiter, store the previous block if it exists	
	  (when block-number
	    (when strkm-debug
	      (message (format "Save %s in blocks " block-number)))
	    (puthash block-number
		     (strkm-save-info-in-blocks block-pre block-info block-content)
		     blocks))
          ;; Reset for the new block
	  (looking-at "^=+\\((\\([0-9]+\\))\\)$")	  
          (setq block-number (match-string 2))  ;; Extract block number (e.g., 0002)
	  (setq block-pre "")
          (setq block-info "")
          (setq block-content "")
	  (setq pre 0)))
       (t
	(cond
	 ((eq pre 0)
	  (setq block-pre (concat block-pre (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position)) "|")))
	 ((eq pre 1)
	  (setq block-info (concat block-info (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)) "|")))
	 (t
          ;; Otherwise, accumulate the content of the current block
          (setq block-content (concat block-content (buffer-substring-no-properties
                                                     (line-beginning-position)
                                                     (line-end-position)) "\n"))))))
      (forward-line 1))
    ;; Store the final block after loop ends
    (when block-number
      (when strkm-debug
	(message (format "Save %s in blocks " block-number)))
      (puthash block-number
	       (strkm-save-info-in-blocks block-pre block-info block-content)
	       blocks))
    ;; Return the hash table
    blocks))

(defun strkm-parse-file (filename)
  "Main function to read and parse the file by splitting it into blocks."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; Call the function to split buffer content into blocks
    (strkm-split-buffer-into-blocks)))

(defun strkm-print-blocks (blocks)
  (maphash (lambda (key value)
             (message "BOOK %s Title: %s\n          Auth : %s\n          Year : %s\n          Publ : %s == %s\n          ISBN: %s\n          Price: %s\n"
		      key
		      (nth 1 value)
		      (nth 0 value)
		      (nth 2 value)
		      (nth 3 value)
		      (nth 4 value)
		      (nth 5 value)
		      (nth 6 value)
		      (nth 7 value)
		      ))
               blocks))

(defun strkm-books-toggle-select ()
  "Toggle the selection status for the current row."
  (interactive)
  ;; Get the current entry in the tabulated list
  (let* ((entry (tabulated-list-get-entry))
         (sel (aref entry 1)))  ;; Retrieve the current value of 'sel' (the selection indicator)
    ;; Invert the selection status
    (if (string= sel "")
        (aset entry 1 ">")      ;; If not selected, mark with '>'
      (aset entry 1 ""))        ;; If selected, clear the selection
    ;; Refresh the display to show updated selection state
    (tabulated-list-print t)))

(defun strkm-books-toggle-select-all (arg)
  "Toggle the selection status for all rows.
If called with C-u, select all rows regardless of their current status."
  (interactive "P")
  ;; Loop through each entry in the tabulated list
  (dolist (entry tabulated-list-entries)
    (let* ((fields (cadr entry))
           (sel (aref fields 1)))  ;; Retrieve the current value of 'sel' for each row
      ;; If 'arg' is non-nil (C-u was pressed), select all rows
      ;; Otherwise, toggle the selection status
      (if arg
          (aset fields 1 ">")     ;; Mark all rows as selected
        ;; Toggle individual selection status
        (if (string= sel "")
            (aset fields 1 ">")   ;; If not selected, mark with '>'
          (aset fields 1 "")))))  ;; If selected, clear the selection
  ;; Refresh the display to reflect changes
  (tabulated-list-print t))


(defun strkm-books-export-csv (filename output nlines)
  "Export the collected OUTPUT data to CSV with FILENAME, including column headers."
  (with-temp-buffer
    ;; Aggiungi l'intestazione delle colonne
    (let ((headers (mapconcat #'car strkm-books-columns strkm-csv-sep)))
      (insert headers "\n"))
    ;; Inserisci le righe di output
    (insert output)
    ;; Salva il file
    (write-file filename)
    (message "Exported %d rows to CSV file: %s" nlines filename)))


(defun strkm-books-export-org (filename output nlines)
  "Export the collected OUTPUT data to ORG format with FILENAME, including column headers."
  (with-temp-buffer
    ;; Inserisci l'intestazione delle colonne
    (let ((headers (concat "| " 
                           (mapconcat #'car strkm-books-columns " | ") 
                           " |\n"))
          (divider (concat "|-" 
                           (mapconcat (lambda (_) (make-string 5 ?-)) strkm-books-columns "-+-") 
                           "-|\n")))
      (insert headers divider))
    ;; Converti output in formato tabella org-mode
    (dolist (line (split-string output "\n" t))
      (let ((org-line (concat "| "
                              (replace-regexp-in-string (regexp-quote strkm-csv-sep) " | " line)
                              " |")))
        (insert org-line "\n")))
    ;; Allinea e salva la tabella
    (org-table-align)
    (write-file filename)
    (message "Exported %d rows to ORG file: %s" nlines filename)))

(defun strkm-books-export-xls (csv-filename xls-filename)
  "Convert CSV-FILENAME to XLS-FILENAME using the Python csv2xls script, including column headers."
  (if (and (executable-find strkm-python-command)
           (file-exists-p (expand-file-name strkm-python-csv2xls-command)))
      (let* ((separator-option (format "-s \"%s\"" strkm-csv-sep))
             (command (format "%s %s %s %s %s"
                              (shell-quote-argument (expand-file-name strkm-python-command))
                              (shell-quote-argument (expand-file-name strkm-python-csv2xls-command))
                              separator-option
                              (shell-quote-argument csv-filename)
                              (shell-quote-argument (expand-file-name xls-filename)))))
        (let ((exit-code (shell-command command)))
          (if (= exit-code 0)
              (message "Exported CSV file %s to XLS file: %s" csv-filename xls-filename)
            (error "Error during conversion: command exited with code %d" exit-code))))
    (error "Python interpreter or csv2xls script not found. Please check `strkm-python-csv2xls-command` and `strkm-python-command` configuration.")))

(require 'cl-lib)

(defun strkm-books-export ()
  "Export selected rows with '>' in the second column to CSV, ORG, or XLS based on the file extension."
  (interactive)
  (let ((filename (read-string "Enter the output file name (with .csv, .org, or .xlsx): "))
        (output "")
        (nlines 0))
    ;; Raccogli tutte le righe selezionate
    (dolist (entry tabulated-list-entries)
      (let ((row (cadr entry))) ;; `row` contiene le colonne della tabella
        ;; Controlla se la seconda colonna contiene '>'
        (when (string-match-p ">" (aref row 1)) ;; Posizione 1 = seconda colonna
          (let ((row-data (mapcar (lambda (col)
                                    (let ((index (+ 2 (cl-position (car col) strkm-books-columns :key #'car))))
                                      (if index
                                          (aref row index)
                                        "")))
                                  strkm-books-columns)))
            ;; Aggiungi la riga all'output
            (cl-incf nlines)
            (setq output (concat output
                                 (mapconcat #'identity row-data strkm-csv-sep)
                                 "\n"))))))
    ;; Determina il formato di esportazione e chiama la funzione appropriata
    (cond
     ;; Export to CSV
     ((string-suffix-p ".csv" filename t)
      (strkm-books-export-csv filename output nlines))
     ;; Export to ORG
     ((string-suffix-p ".org" filename t)
      (strkm-books-export-org filename output nlines))
     ;; Export to XLSX by first creating a CSV file and converting it
     ((string-suffix-p ".xlsx" filename t)
      (let ((csv-temp-file (make-temp-file "strkm-export" nil ".csv")))
        ;; Export to CSV first
        (strkm-books-export-csv csv-temp-file output nlines)
        ;; Then convert to XLSX
        (strkm-books-export-xls csv-temp-file filename)
        ;; Clean up temporary CSV file
        (delete-file csv-temp-file)))
     ;; Estensione non supportata
     (t
      (message "Unsupported file extension. Please use '.csv', '.org', or '.xlsx'.")))))


(defun strkm-books-print-table ()
  "Display the tabulated list with selected rows in bold."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tabulated-list-init-header)
    (tabulated-list-print)))


(defun strkm-dont-prompt-save-books ()
  "Disable save prompt for .books buffers."  
  (when (and buffer-file-name (string-match "\\.books\\'" buffer-file-name))
    (set-buffer-modified-p nil)
    )
  t)


(add-hook 'kill-buffer-query-functions 'strkm-dont-prompt-save-books)


;; Automatically enable strkm-books-mode for files with .books extension
(add-to-list 'auto-mode-alist '("\\.books\\'" . strkm-books-mode))

(defun remove-invalid-utf8-chars ()
  "Replace all characters in the current buffer that cannot be represented in UTF-8 with '?'."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (let ((char (char-after)))
      ;; Verifica se il carattere è codificabile in UTF-8 con lunghezza 1
      (if (or (null char) ;; Salta posizioni vuote
              (condition-case nil
                  (let ((encoded (encode-coding-string (string char) 'utf-8)))
                    (and encoded (= (length encoded) 1))) ;; Valido solo se la stringa codificata ha lunghezza 1
                (error nil))) ;; Se la codifica fallisce, è non rappresentabile
          (forward-char 1) ;; Carattere valido, sposta al prossimo
        ;; Carattere non rappresentabile
        (delete-char 1)
        (insert "?")))))


(defun strkm-process-message (input-file &optional recode books-file)
  "Processa il file di testo specificato.
1. Legge il file con codifica windows-1252 (se RECODE è non-nil).
2. Cancella tutto il contenuto dall'inizio fino alla stringa 'ITRBI' seguita da due linee vuote e una riga di '='.
3. Se BOOKS-FILE è fornito, usa quello come nome del file di output.
4. Se BOOKS-FILE non è fornito, chiede il nome del file di output, proponendo un valore predefinito basato sulla data attuale.
5. Se il file di output esiste, chiede conferma prima di sovrascriverlo.
6. Salva il file in codifica utf-8, sostituendo caratteri non rappresentabili con '?'."
  (interactive "fSeleziona il file di input: \nP")
  (let* ((current-date (format-time-string "%Y_%m_%d"))
         (default-output-file (or books-file (format "%s.books" current-date)))
         (output-file (if books-file
                          books-file
                        (read-file-name "Nome del file di output: " nil nil nil default-output-file))))
    ;; Conferma se il file esiste
    (when (and (file-exists-p output-file)
               (not (yes-or-no-p (format "Il file '%s' esiste. Sovrascriverlo? " output-file))))
      (user-error "Operazione annullata. Il file non è stato sovrascritto."))
    (with-temp-buffer
      ;; Leggi il file
      (insert-file-contents input-file)
      ;; Esegui il recode se necessario
      (unless recode
        (recode-region (point-min) (point-max) 'windows-1252 'utf-8))
      ;; Rimuovi i caratteri non rappresentabili in UTF-8
      (remove-invalid-utf8-chars)
      ;; Vai all'inizio del buffer e cerca la stringa con il pattern richiesto
      (goto-char (point-min))
      (if (re-search-forward "^\\(=\\{64\\}(0001)\\)" nil t)
          ;; Rimuovi tutto prima del match
          (delete-region (point-min) (match-beginning 0))
        (message "Stringa 'ITRBI' seguita da due linee vuote e una riga di '=' non trovata."))
      ;; Salva il file con codifica UTF-8
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) output-file))
      (message "File processato e salvato come: %s" output-file)
      output-file)))

(defun strkm-process-and-export-xls (input-file)
  "Process a .books file and export it directly to XLS format.
If the input file name contains a date, use it in the output file name.
Supports formats: YYYY_MM_DD and DD-MMM-YY (e.g., 25-Nov-24)."
  (interactive "fSelect input file to process: ")
  (let* ((date-in-filename
          (cond
           ;; Cerca il formato YYYY_MM_DD
           ((string-match "\\([0-9]\\{4\\}_[0-9]\\{2\\}_[0-9]\\{2\\}\\)" input-file)
            (match-string 1 input-file))
           ;; Cerca il formato DD-MMM-YY
           ((string-match "\\([0-9]\\{2\\}-[A-Za-z]\\{3\\}-[0-9]\\{2\\}\\)" input-file)
            ;; Converte il formato in YYYY_MM_DD
            (let* ((date-str (match-string 1 input-file))
                   (day (substring date-str 0 2))
                   (month-str (substring date-str 3 6))
                   (year (substring date-str 7 9))
		    (month-num (cond
				((string= month-str "Jan") "01")
				((string= month-str "Feb") "02")
				((string= month-str "Mar") "03")
				((string= month-str "Apr") "04")
				((string= month-str "May") "05")
				((string= month-str "Jun") "06")
				((string= month-str "Jul") "07")
				((string= month-str "Aug") "08")
				((string= month-str "Sep") "09")
				((string= month-str "Oct") "10")
				((string= month-str "Nov") "11")
				((string= month-str "Dec") "12")
				(t nil)))) ;; Caso predefinito per errori
              (if month-num
                  (format "20%s_%s_%s" year month-num day) ;; Converte in formato YYYY_MM_DD
                nil)))
           ;; Usa la data corrente se nessuna data è presente
           (t (format-time-string "%Y_%m_%d"))))
         (default-output (concat date-in-filename ".xlsx"))
         (output-file (read-file-name "Enter XLS output file name: " nil nil nil default-output))
         (books-file (concat (file-name-directory output-file)
                             (concat date-in-filename ".books")))
         (processed-file (strkm-process-message input-file nil books-file)) ;; Always recode
         (data (strkm-parse-file processed-file))
         (csv-temp-file (make-temp-file "strkm-export" nil ".csv")))
    ;; Scrive il file .books nella stessa directory dell'output XLS
    ;; (copy-file processed-file books-file t)
    ;; Create a CSV file from the processed data
    (with-temp-buffer
      ;; Insert column headers
      (insert (mapconcat (lambda (col) (nth 0 col)) strkm-books-columns strkm-csv-sep) "\n")
      ;; Insert rows
      (insert (mapconcat
               (lambda (entry)
                 (let ((row (cdr entry)))
                   (mapconcat #'identity row strkm-csv-sep)))
               (strkm-hash-table-to-list data) "\n"))
      (write-region (point-min) (point-max) csv-temp-file))
    ;; Convert the CSV to XLS
    (strkm-books-export-xls csv-temp-file output-file)
    ;; Clean up temporary CSV file
    (delete-file csv-temp-file)
    (message "Export completed: %s\nBooks file saved: %s" output-file books-file)))


(defun strkm-remove-menu-and-subitems (menu-name parent-menu)
  "Remove a menu named MENU-NAME and all its subcommands from the PARENT-MENU.
MENU-NAME is the name of the menu to remove.
PARENT-MENU is the path to the parent menu, e.g., [menu-bar tools]."
  (interactive "sEnter menu name to remove: \nxEnter parent menu path: ")
  (let ((menu-key (vconcat parent-menu (vector menu-name))))
    (when (lookup-key global-map menu-key)
      ;; Rimuovi tutte le sottovoci
      (mapatoms
       (lambda (sym)
         (let ((subkey (vconcat menu-key (vector sym))))
           (when (lookup-key global-map subkey)
             (define-key global-map subkey nil)))))
      ;; Rimuovi il menu principale
      (define-key global-map menu-key nil)
      (message "Menu '%s' and its subcommands have been removed." menu-name))))

(defun strkm-remove-library-menu ()
  "Remove the 'Library' menu and all its subcommands from the 'Tools' menu."
  (interactive)
  (strkm-remove-menu-and-subitems 'library [menu-bar tools]))

(strkm-remove-library-menu)

;; Aggiungi un sottomenu "Library" sotto "Tools" solo se non esiste
(when (not (lookup-key global-map [menu-bar tools library]))
  (define-key global-map [menu-bar tools library]
    (cons (format "Library (%s)" strkm-version) (make-sparse-keymap "Library"))))

;; Aggiungi il comando "Process Starkman Message" al sottomenu "Library" solo se non esiste
(when (not (lookup-key global-map [menu-bar tools library process-starkman-message]))
  (define-key global-map [menu-bar tools library process-starkman-message]
    '(menu-item "Process Starkman Message"
                (lambda ()
                  (interactive)
                  (call-interactively #'strkm-process-message))
                :help "Processa un messaggio Starkman e salva in formato .books")))

;; Aggiungi il comando "Process and Export to XLS" al sottomenu "Library"
(when (not (lookup-key global-map [menu-bar tools library process-and-export-xls]))
  (define-key global-map [menu-bar tools library process-and-export-xls]
    '(menu-item "Process and Export to XLS"
                (lambda ()
                  (interactive)
                  (call-interactively #'strkm-process-and-export-xls))
                :help "Processa un file Starkman e salva direttamente in formato XLS")))

(defun strkm-reload-package ()
  "Reload the `strkm` package from the given URL.
If no URL is provided, use `strkm-default-package-url`."
  (interactive)
  (load-elisp-from-url strkm-default-package-url)
  (message "Package successfully reloaded from: %s" url))
  

(provide 'strkm-books-mode)
;;; strkm-books-mode.el ends here
