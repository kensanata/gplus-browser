(defun gplus-browse-archive (dir)
  "Browse the archive in directory DIR.
An archive is the uncompressed directory,
\"~/Downloads/Takeout/Google+ Stream/Posts\" or similar."
  (interactive "DWhere is the archive? ")
  (let ((files (directory-files dir 'full "\\.json\\'")))
    (switch-to-buffer (get-buffer-create "*G+ Archive*"))
    (gplus-browser-mode)))

(defvar gplus-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'gplus-preview-file)
    (define-key map (kbd "C-c C-c") 'gplus-post-other-buffer)
    map))

(define-derived-mode gplus-browser-mode tabulated-list-mode "Google+"
  "Major mode to view the list of files in your archive.
This requires the variable `files' to be dynamically bound to a list
of absolute paths.

\\{gplus-browser-mode-map}"
  (setq tabulated-list-format
	(vector '("Date" 10 t)
		'("Entry" 60))
	tabulated-list-sort-key '("Date")
	tabulated-list-entries
	(mapcar 'gplus-filename-data files))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun gplus-filename-data (file)
  "Extract info from filename FILE."
  (if (string-match "/\\([0-9]+\\) - \\(.*\\)\\.json\\'" file)
      (let ((date (match-string 1 file))
	    (text (match-string 2 file)))
	(list file (vector date (replace-regexp-in-string "_" "'" text))))
    (list file ["????????" "Unknown"])))

(defvar gplus-file nil
  "The json filename of this buffer.")

(defun gplus-preview-file ()
  "Preview file at point."
  (interactive)
  (let ((file (get-text-property (point) 'tabulated-list-id))
	(buf (get-buffer-create  "*G+ Post*")))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert (gplus-content (json-read-file file))))
      (gplus-preview-mode)
      (setq-local gplus-file file))
    (display-buffer buf)))

(defun gplus-content (json)
  "Render JSON content as Markdown."
  (let ((text (html-to-markdown-string (cdr (assq 'content json))))
	(reshared (cdr (assq 'resharedPost json)))
	(comments (cdr (assq 'comments json)))
	(list '("")))
    ;; add comments from last to first
    (dotimes (i (length (reverse comments)))
      (let* ((comment (aref comments i))
	     (author (cdr (assq 'author comment)))
	     (display-name (cdr (assq 'displayName author)))
	     (text (html-to-markdown-string (cdr (assq 'content comment)))))
	(setq list (cons (format "%s\n\n– %s\n" text display-name) list))))
    ;; separator in front of the first comment
    (when comments
      (setq list (cons "\n----\n" list)))
    ;; now comes the text
    (when text
      (setq list (cons text list)))
    ;; reshared posts gets added to the front, in quotes
    (when reshared
      (let* ((author (cdr (assq 'author reshared)))
	     (display-name (cdr (assq 'displayName author)))
	     (text (html-to-markdown-string (cdr (assq 'content reshared)))))
	(setq list (cons (format "[quote]\n%s\n\n– %s\n[/quote]\n"
				 text display-name)
			 list))))
    (mapconcat 'identity (mapcar 'gplus-strip list) "\n")))

(defun gplus-strip (str)
  "Remove extra stuff from the Markdown generated."
  (replace-regexp-in-string "^<br>$" "" str))

(defvar gplus-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)
    (define-key map (kbd "C-c C-c") 'gplus-post-buffer)
    map))
	
(define-derived-mode gplus-preview-mode view-mode "G+ View"
  "Major mode to view JSON files in your G+ Archive."
  (message "Edit and post when you're ready."))

(defun gplus-post-other-buffer ()
  "Post the preview buffer to an Oddmuse wiki."
  (interactive)
  (let ((buf (get-buffer "*G+ Post*")))
    (unless buf
      (error "You need to preview a file, first"))
    (with-current-buffer buf
      (call-interactively 'gplus-post-buffer))))
  
(defun gplus-post-buffer (name)
  "Post the current buffer to an Oddmuse wiki."
  (interactive
   (let* ((regexp "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)")
	  (date (when (string-match regexp gplus-file)
		  (concat (match-string 1 gplus-file) "-"
			  (match-string 2 gplus-file) "-"
			  (match-string 3 gplus-file) " "))))
     (list (read-string "Page name: " date))))
  (message "Saving: %s" name))
