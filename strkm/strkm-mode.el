;;; strkm-books-mode.el --- A major mode for managing .books files -*- lexical-binding: t; -*-

;; Author: Emmanuele Somma <emmanuele@exedre.org>
;; Maintainer: Emmanuele Somma <emmanuele@exedre.org>
;; Created: 2024-11-06
;; Version: 0.1
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
    (define-key map (kbd "C-c C-r") 'strkm-books-toggle-view)  ;; Toggle between views
    (define-key map (kbd "<down>") 'strkm-books-next)  
    (define-key map (kbd "<up>") 'strkm-books-previous)
    map)
  "Keymap for `strkm-books-mode'.")

(defvar strkm-books-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)  
    (define-key map [mouse-1] 'strkm-books-click-action)  
    (define-key map (kbd "n") 'strkm-books-next-row)  
    (define-key map (kbd "p") 'strkm-books-previous-row)
    (define-key map (kbd "<down>") 'strkm-books-next-row)  
    (define-key map (kbd "<up>") 'strkm-books-previous-row)
    (define-key map (kbd "SPC") 'strkm-books-toggle-select)  ;; Toggle selection with 'SPC'
    (define-key map (kbd "C-a") 'strkm-books-toggle-select-all)  ;; 
    (define-key map (kbd "C-e") 'strkm-books-export) ;; Export to CSV with 'C-c C-e'
    map)
  "Keymap for `strkm-books-tabulated-mode'.")

(define-key strkm-books-mode-map [mouse-1] 'strkm-books-click-action)
(define-key strkm-books-mode-map [drag-mouse-1] 'strkm-books-click-action)
(define-key strkm-books-tabulated-mode-map [mouse-1]    'strkm-books-tabulated-click-action)


(require 'tabulated-list)

(defun strkm-books-display-tabulated ()
  "Display the contents of the .books file in a spreadsheet-like table format using tabulated-list-mode."
  (interactive)
  ;; Save buffer-local variables for the window and file name
  (setq strkm-source-file buffer-file-name)
	
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
        ;; (setq tabulated-list-sort-key (cons "ID" nil)) ;; Sostituisci "ID" con il nome della colonna desiderata per il primo ordinamento	
        (tabulated-list-sort 0)
	
	(tabulated-list-print)
	
	(display-buffer (current-buffer))
	(other-window 1)	
	))))

(defun strkm-tabulated-list-get-current-row ()
  "Esegue un'azione sulla riga corrente della tabella."
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
         (id (aref entry 0))  ;; Assume the ID is the first element
	 (source-file strkm-source-file))
    (if (one-window-p)
        (progn
          (split-window-below)  ;; Se non c'è una seconda finestra, la crea
          (find-file source-file))  ;; Apri il file .books
      (other-window 1))
    (goto-char (point-min))  ;; Inizia dall'inizio del file
    (strkm-highlight-text-block id)
    (goto-char (point-min))  ;; Inizia dall'inizio del file
    (search-forward (format "================================================================(%s)" id) nil t)
    (recenter-top-bottom 0)
    (other-window 1)  ;; Torna alla finestra originale
    ))

(defun strkm-hash-table-to-list (hash-table)
  "Convert a hash table to an alist."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))

(defun strkm-books-next ()
  "Sposta il cursore alla scheda successiva nel buffer e evidenzia il blocco corrente."
  (interactive)
  (let ((current (point)))  ;; Memorizza la posizione attuale del cursore
    (if (re-search-forward "^=+\\s-*(\\([0-9]+\\))" nil t)
        (progn
          (goto-char (match-beginning 0))  ;; Sposta il cursore all'inizio della scheda trovata
	  (forward-line)
          (strkm-highlight-current-block))  ;; Evidenzia il blocco corrente
      (goto-char current)  ;; Torna alla posizione originale se non viene trovata nessuna scheda
      (message "Nessuna scheda successiva trovata."))))

(defun strkm-books-previous ()
  "Sposta il cursore alla scheda precedente nel buffer e evidenzia il blocco corrente."
  (interactive)
  (let ((current (point)))  ;; Memorizza la posizione attuale del cursore    
    (if (re-search-backward "^=+\\s-*(\\([0-9]+\\))" nil t)
        (progn
          (goto-char (match-beginning 0))  ;; Sposta il cursore all'inizio della scheda trovata
	  (let ((current (point)))  ;; Memorizza la posizione attuale del cursore    
	    (if (re-search-backward "^=+\\s-*(\\([0-9]+\\))" nil t)
		(progn
		  (goto-char (match-beginning 0))  ;; Sposta il cursore all'inizio della scheda trovata
		  (forward-line)
		  (strkm-highlight-current-block))  ;; Evidenzia il blocco corrente
	      )))
      (goto-char current)  ;; Torna alla posizione originale se non viene trovata nessuna scheda
      (message "Nessuna scheda precedente trovata."))))


(defun kill-this-buffer ()
  "Kill this buffer and get rid of window"
  (interactive)
  (kill-buffer)
  (delete-window))

(defun strkm-books-tabulated-click-action (event)
  "Azione da mettere in atto per il click nella book-view"
  (interactive "e")
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      ;; Aggiungi qui l'azione da eseguire
      (strkm-books-jump-to-current)
      (message "Hai cliccato sulla riga con ID: %s" (aref entry 0)))))

(defun strkm-books-click-action (event)
  "Azione da mettere in atto per il click nella book-view"
  (interactive "e")
  ;; Assicurati che l'evento di mouse sia su una posizione corretta
  (let* ((posn (event-start event))
         (buffer (window-buffer (posn-window posn)))
         (position (posn-point posn)))
    ;; Verifica che siamo nella tabella e prendi l'ID dell'elemento cliccato
    (with-current-buffer buffer
      (goto-char position)
      (let ((id (strkm-highlight-current-block)))
	(message "Hai cliccato sulla riga con ID: %s" id)))))



;; (debug-on-entry (symbol-function 'strkm-books-click-action-book))

(defun strkm-highlight-current-block ()
  "Enfatizza il blocco di testo in cui si trova il cursore nel file .books.
Il blocco verrà messo in grassetto. Rimuove prima l'enfatizzazione da tutto il file."
  (interactive)

  (let ((inhibit-read-only t))  ;; Permette modifiche nel buffer
    ;; Trova la riga superiore del blocco
    (save-excursion
      (let* ((start (re-search-backward "^=+(\\([0-9]+\\))" nil t))
             (id (if start (match-string 1))))  ;; Usa match-string per ottenere l'ID
        ;; Trova la riga inferiore del blocco
        (when start
	    (progn
	      (goto-char (point-min))
              (strkm-highlight-text-block id))))
      id)))

(defun strkm-highlight-text-block (number)
  "Enfatizza il blocco di testo tra le righe di delimitazione contenenti il NUMBER fornito.
Il blocco verrà messo in grassetto. Rimuove prima l'enfatizzazione da tutto il file."
  (interactive "sInserisci il numero da enfatizzare: ")
  
  ;; Rimuovi l'enfatizzazione da tutto il buffer
  (read-only-mode 0)
  (remove-text-properties (point-min) (point-max) '(face nil))

  ;; Trova il blocco di testo da enfatizzare
  (goto-char (point-min))
  (let ((start (search-forward (format "================================================================(%s)" number)))
        (end (search-forward "================================================================"))
        (inhibit-read-only t))  ;; Permette modifiche nel buffer
    (when (and start end)
      ;; Aggiungi i delimitatori di grassetto
      (goto-char start)
      (recenter-top-bottom 0)
      (forward-line -1)  ;; Torna alla fine della riga
      (let ((block-start (point)))
        (goto-char end)
        (forward-line -1)  ;; Torna alla fine della riga
        ;; Inizia a segnare il blocco di testo
        (add-text-properties block-start (point) '(face (:weight bold)))
	(read-only-mode 1)	
        (message "Blocco di testo con numero %s enfatizzato." number)))))


(easy-menu-define strkm-books-menu strkm-books-mode-map
  "Menu for `strkm-books-mode'."
  '("Books"  ;; Nome del menu
    ["Display Table View" books-display-table t]  ;; Voce per visualizzare la tabella
    ["Toggle View" strkm-books-toggle-view t]     ;; Voce per alternare la visualizzazione
    "---"  ;; Separatore
    ["Quit" kill-this-buffer t]))  ;; Voce per chiudere il buffer

;;;###autoload
(define-derived-mode strkm-books-mode text-mode "Books"
  "Major mode for editing and viewing .books files."
  :syntax-table nil
  :abbrev-table nil
  (read-only-mode 1)
  (easy-menu-add strkm-books-menu)  ;; Aggiungi il menu personalizzato
  (use-local-map strkm-books-mode-map))

(defun strkm-x-string-trim (str)
  str)

(defun strkm-extract-title-authors-or-editors (line)
  "Extract the title and authors (or editors) from the LINE.
If the line contains '/ ed. by', it assumes that the part before that
is the title and the part after it (until '0.-') is the authors or editors.
Otherwise, it assumes the first part before '|' is the author and the
rest of the line is the title, replacing '|' with spaces."
  (let (title author ed)
    (if (string-match "/[| ]+\\(ed\\.[| ]+\\)?by[| ]+\\(.*?\\)[| ]+0\\.-|$" line)
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
            (setq title (replace-regexp-in-string "|" " " (match-string 2 line))))))
    ;; Return the results as a list
    (list (strkm-x-string-trim title) (strkm-x-string-trim author) ed)))

(defun strkm-extract-isbn-format (line)
  "Extract a valid ISBN (10 or 13 digits) from the LINE after the last '|'."
  (let ((isbn "-") (fmt "-") (price "-"))
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
  (let (where publisher year isbn price)
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

(defun strkm-parse-file-to-csv ()
  "Interactive function to parse an input file and create an output CSV file."
  (interactive) ;; Makes the function callable by the user
  (let ((input-file (read-file-name "Enter the input file path: "))	
        (output-file (if strkm-make-csv
		       (read-file-name "Enter the output CSV file path: ")
		       )))
    ;; Open and parse the input file
    (let ((blocks (strkm-parse-file input-file)))
      ;;(hash-table-keys blocks)
      ;; After parsing all the lines, export the parsed data to CSV
      (when strkm-print
	(strkm-print-blocks blocks))
      (when strkm-make-csv
	(strkm-export-to-csv blocks output-file)))))

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
  (let* ((entry (tabulated-list-get-entry))
         (sel (aref entry 1)))  ;; Ottieni il valore attuale di 'sel'
    ;; Inverti lo stato di selezione
    (if (string= sel "")
        (aset entry 1 ">")  ;; Se non è selezionato, metti 'X'
      (aset entry 1 ""))    ;; Se è selezionato, rimuovi la selezione
    ;; Aggiorna la visualizzazione
    (tabulated-list-print t)))

(defun strkm-books-toggle-select-all (arg)
  "Toggle the selection status for all rows.
If called with C-u, select all rows regardless of their current status."
  (interactive "P")
  (dolist (entry tabulated-list-entries)
    (let* ((fields (cadr entry))
           (sel (aref fields 1)))  ;; Ottieni il valore attuale di 'sel'
      ;; Se arg è non-nil (C-u è stato premuto), seleziona tutti
      ;; altrimenti, toggla lo stato di selezione
      (if arg
          (aset fields 1 ">")  ;; Seleziona tutti
        ;; Toggle stato di selezione
        (if (string= sel "")
            (aset fields 1 ">")  ;; Se non è selezionato, metti '>'
          (aset fields 1 "")))))  ;; Se è selezionato, rimuovi la selezione
  ;; Aggiorna la visualizzazione
  (tabulated-list-print t))

(defun strkm-books-export ()
  "Export selected rows with '>' in the second column to CSV or ORG based on the file extension."
  (interactive)
  (let ((filename (read-string "Enter the output file name (with .csv or .org): "))
        (output "")
	(nlines 0))
    ;; Raccogli tutte le righe selezionate
    (dolist (entry tabulated-list-entries)
      (let ((row (cadr entry))) ;; `row` contiene le colonne della tabella
        ;; Controlla se la seconda colonna contiene '>'
        (when (string-match-p ">" (aref row 1)) ;; Posizione 1 = seconda colonna
          (let ((row-data (vector (aref row 0)  ;; Prima colonna
                                  (aref row 2)  ;; Terza colonna
                                  (aref row 3)  ;; Quarta colonna
                                  (aref row 4)  ;; Quinta colonna
                                  (aref row 5)  ;; Sesta colonna
                                  (aref row 6)  ;; Settima colonna
                                  (aref row 7)  ;; Ottava colonna
                                  (aref row 8)  ;; Nona colonna
                                  (aref row 9)))) ;; Decima colonna
            ;; Inserisci l'entry nel buffer di output come stringa separata
	    (cl-incf nlines)
            (setq output (concat output
                                 (mapconcat 'identity (append row-data nil) strkm-csv-sep)
                                 "\n"))))))    
    ;; Determina il formato di esportazione e salva il file
    (with-temp-buffer
      (cond
       ;; Esportazione in CSV
       ((string-suffix-p ".csv" filename t)
        (insert output)
        (write-file filename)
        (message "Exported %d rows to CSV file: %s" nlines filename))
       ;; Esportazione in ORG usando org-mode
       ((string-suffix-p ".org" filename t)
        ;; Inserisci l'intestazione per la tabella org-mode
        (insert "| ID | Authors | Type | Title | City | Publisher | Year | ISBN | Format | Currency | Price |\n")
        (insert "|----+---------+------+-------+------+-----------+------+-------+--------+----------+-------|\n")
        ;; Trasforma l'output in formato org-mode con "|"
        (dolist (line (split-string output "\n" t))
          (let ((org-line (concat "| " 
                                  (replace-regexp-in-string (regexp-quote strkm-csv-sep) " | " line)
                                  " |")))
            (insert org-line "\n")))
        ;; Allinea la tabella org-mode e scrivi il file
        (org-table-align)
        (write-file filename)
        (message "Exported %d rows to ORG file: %s" nlines filename))
       ;; Estensione non supportata
       (t
        (message "Unsupported file extension. Please use '.csv' or '.org'."))))))

  
(defun strkm-books-print-table ()
  "Display the tabulated list with selected rows in bold."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tabulated-list-init-header)
    (tabulated-list-print)))


;; Automatically enable strkm-books-mode for files with .books extension
(add-to-list 'auto-mode-alist '("\\.books\\'" . strkm-books-mode))



(provide 'strkm-books-mode)
;;; strkm-books-mode.el ends here
