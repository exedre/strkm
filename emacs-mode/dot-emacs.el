
(require 'url)  ;; Assicura la disponibilità della libreria `url.el`

(defun load-elisp-from-url (url)
  "Load and evaluate an Emacs Lisp file from a given URL."
  (let ((buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        ;; Cerca la fine dell'header HTTP, delimitato da una linea vuota
        (goto-char (point-min))
        (if (search-forward "\n\n" nil t)
            (progn
              ;; Rimuovi gli header HTTP e valuta il contenuto del buffer
              (delete-region (point-min) (point))
              (eval-buffer)
              (message "Successfully loaded and evaluated the file from URL: %s" url))
          (message "Failed to parse HTTP headers"))
        ;; Chiudi il buffer temporaneo
        (kill-buffer buffer)))))

;; Esempio di utilizzo
(load-elisp-from-url "http://raw.githubusercontent.com/exedre/strkm/refs/heads/main/strkm/strkm-mode.el")
