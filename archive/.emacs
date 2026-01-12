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

(load-elisp-from-url "http://osiride-public/~h856605/strkm/strkm-main/strkm/strkm-mode.el")

(defun ensure-csv2xls-script ()
  "Ensure the presence of the `csv2xls.py` script in ~/bin, creating the directory and downloading the file if necessary."
  (interactive)
  (let* ((bin-dir (expand-file-name "bin" (getenv "HOME")))
         (script-name "csv2xls.py")
         (script-path (expand-file-name script-name bin-dir))
         (script-url "http://osiride-public/~h856605/strkm/strkm-main/strkm/csv2xls.py"))
    ;; Check if bin directory exists, if not create it.
    (unless (file-directory-p bin-dir)
      (make-directory bin-dir t)
      (message "Directory %s created." bin-dir))
    ;; Check if the script exists, if not download it.
    (unless (file-exists-p script-path)
      (url-copy-file script-url script-path t)
      (message "Script %s downloaded to %s." script-name bin-dir))))

;; Add the function to Emacs startup
(add-hook 'emacs-startup-hook #'ensure-csv2xls-script)

(require 'strkm-books-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(strkm-python-command "c:/Program Files/python37/python.exe")
 '(strkm-python-csv2xls-command "d:/Dati/emacs-home/bin/csv2xls.py"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(define-key global-map [menu-bar tools process-text-file]
  (cons "Process Book File" 
        (lambda ()
          (interactive)
          (call-interactively #'process-text-file))))

(define-key global-map [menu-bar tools separator-process-text-file]
  '(menu-item "--")) ;; Aggiunge un separatore


