;; Copyright (C) 2018  Alex Schroeder <alex@gnu.org>

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Use Google Takeout to download an archive of your Goole+ Stream.
;; Unpack it. You should get a directory full of images and JSON
;; files, e.g. ~/Downloads/Takeout/Google+ Stream/Posts. With that in
;; place, run M-x gplus-browse-archive and point it that that Posts
;; directory.

;; Use the up and down arrows to pick a post. Use RET to view it. Edit
;; it, if you need to. Use C-c C-c to post it to an Oddmuse wiki.

;;; Code

(require 'tabulated-list)
(require 'markdown-mode)
(require 'html-to-markdown)
(require 'oddmuse-curl)

;; The G+ Archive buffer

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
    (define-key map (kbd "d") 'gplus-trash-file)
    (define-key map (kbd "q") 'gplus-quit)
    (define-key map (kbd "SPC") 'gplus-scroll-up)
    (define-key map (kbd "C-c C-c") 'gplus-post-other-buffer)
    map))

(defun gplus-scroll-up ()
  "Scroll *G+ Post*, then go to the next post in *G+ Archive*."
  (interactive)
  (if (and (get-buffer-window "*G+ Post*")
	   (with-current-buffer "*G+ Post*"
	     (and (window-end)
		  (> (point-max) (window-end (get-buffer-window "*G+ Post*"))))))
      (let ((other-window-scroll-buffer (get-buffer "*G+ Post*")))
	(message "%S" (scroll-other-window)))
    (next-line 1)
    (gplus-preview-file)))

(defun gplus-quit ()
  "Bury *G+ Post*, then quit *G+ Archive*."
  (interactive)
  (let ((window (get-buffer-window "*G+ Post*")))
    (if (not window)
	(bury-buffer)
      (select-window window)
      (kill-buffer-and-window))))

(defun gplus-trash-file (file)
  "Delete FILE from the archive.
This calls `delete-file'. When `delete-by-moving-to-trash' is
non-nil, the file is moved to the trash instead."
  (interactive
   (list (get-text-property (point) 'tabulated-list-id)))
  (unless file
    (error "This only works in the *G+ Archive* buffer"))
  (tabulated-list-delete-entry)
  (delete-file file)
  ;; move up if this was the last file
  (when (eobp)
    (previous-line))
  ;; and preview the next
  (gplus-preview-file))

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

;; The G+ Preview buffer

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
	(insert (gplus-content (json-read-file file)))
	(goto-char (point-min)))
      (gplus-preview-mode)
      (setq-local gplus-file file))
    (display-buffer buf)))

(defun gplus-content (json)
  "Render JSON content as Markdown."
  (let ((text (cdr (assq 'content json)))
	(reshared (cdr (assq 'resharedPost json)))
	;; add comments from last to first
	(comments (reverse (cdr (assq 'comments json))))
	(link (cdr (assq 'link json)))
	(list '("")))
    (dotimes (i (length comments))
      (let* ((comment (aref comments i))
	     (author (cdr (assq 'author comment)))
	     (display-name (or (cdr (assq 'displayName author)) "Unknown"))
	     (date (cdr (assq 'creationTime comment)))
	     (text (html-to-markdown-string (or (cdr (assq 'content comment)) ""))))
	(setq date (if date (substring date 0 10) ("date unknown")))
	(setq list (cons (format "%s\n\n– %s %s\n" text display-name date)
			 list))
	;; separator before it
	(setq list (cons "----\n" list))))
    ;; now comes the text
    (when text
      (setq list (cons (format "%s\n" (html-to-markdown-string (or text ""))) list)))
    ;; reshared posts gets added to the front, in quotes
    (when reshared
      (let* ((author (cdr (assq 'author reshared)))
	     (display-name (or (cdr (assq 'displayName author)) "Unknown"))
	     (text (html-to-markdown-string (or (cdr (assq 'content reshared)) ""))))
	(setq list (cons (format "[quote]\n%s\n\n– %s\n[/quote]\n"
				 text display-name)
			 list))))
    ;; and finally, the link (i.e. at the very top)
    (when link
      (let* ((title (cdr (assq 'title link)))
	     (url (cdr (assq 'url link))))
	(setq list (cons (format "[%s](%s)\n" title url)
			 list))))
    (dolist (func '(gplus-italic gplus-strip gplus-fix-links gplus-fix-line-ends))
      (setq list (mapcar func list)))
    (mapconcat 'identity list "\n")))

(defun gplus-italic (str)
  "Translate _foo_ into *foo*."
  (replace-regexp-in-string "_\\([^_\n]+\\)_" "*\\1*" str))

(defun gplus-strip (str)
  "Remove extra stuff from the Markdown generated."
  (replace-regexp-in-string "^<br>$" "" str))

(defun gplus-fix-links (str)
  "Somehow the links have extra double quotes."
  (replace-regexp-in-string "(\"\\(\\S-+\\)\")" "(\\1)" str))

(defun gplus-fix-line-ends (str)
  "Get rid of those two spaces at the end of lines."
  (replace-regexp-in-string "  $" "" str))

(defvar gplus-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-c") 'gplus-post-buffer)
    (define-key map (kbd "C-c C-t") 'oddmuse-tag)
    map))

(defvar oddmuse-tags nil
  "Favorite tags to use for completion.
This is used for `completing-read'. It's usually a list of
strings.")

(defun oddmuse-tag (&rest tags)
  (interactive
   (let ((tags nil))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "\\[\\[tag:\\(.*?\\)\\]\\]" nil t)
	 (setq tags (cons (match-string 1) tags))))
     (let ((tag (completing-read "Tag: " oddmuse-tags
				 (lambda (s)
				   (not (member s tags))))))
       (while (not (string= tag ""))
	 (setq tags (cons tag tags))
	 (setq tag (completing-read "Tag: " oddmuse-tags
				    (lambda (s)
				      (not (member s tags)))))))
     tags))
  (save-excursion
    (when tags
      (goto-char (point-min))
      (when (re-search-forward "Tags:\\( \\[\\[tag:.*?\\]\\]\\)+\n" nil t)
	(replace-match ""))
      (goto-char (point-max))
      ;; make sure a new paragraph starts
      (unless (and (> (point) 1)
		   (string= "\n\n" (buffer-substring (- (point) 2) (point))))
	(newline (if (eq (char-before) ?\n) 1 2)))
      (insert "Tags:")
      (mapcar (lambda (s)
		(insert " [[tag:" s "]]"))
	      tags))))

(define-derived-mode gplus-preview-mode markdown-mode "G+ View"
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
  (let ((oddmuse-page-name name))
    (call-interactively 'oddmuse-post)))

(provide 'gplus-browser)
