;;; package -- Utilities to migrate denote notes to logseq
;;
;; Author: hapst3r
;; Version: 0.1
;; URL: https://github.com/bitspook/notes-migrator/denote-to-logseq.el
;; Keywords: Denote, Logseq
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (denote "1.0.0")
;;
;;; Commentary:
;; A WIP package for migrating Denote notes to Logseq
;;
;;; Code:

(require 'denote)

(defvar logseq-directory nil)

;;;; Copy contents of DENOTE directory to LOGSEQ directory
(defun nm--copy-org-files-from-to (src dest)
  "Copy all Org files from SRC to DEST.
SRC is supposed to be denote's, DEST logseq's notes directory."
  ;; TODO: Rewrite using mechanisms provided by DENOTE
  (let ((contents (directory-files src t "org$")))
    (mapcar #'(lambda (file)
		            (copy-file file dest t))
	          contents)))

(defun nm--convert-denote-links-to-logseq ()
  "Convert all denote links to logseq links in current buffer.
Current-buffer should be in `org-mode'."
  (let ((denote-link-rx (rx "[[denote:")))
    (while (re-search-forward denote-link-rx nil t)
	    (let* ((element (org-element-copy (org-element-context)))
	           (contents (org-element-property :format element))
	           (linked-id (org-element-property :path element))
	           (linked-file (denote-get-path-by-id linked-id))
	           (linked-title (denote-retrieve-title-value linked-file 'org))
	           (logseq-link (format "[[%s]] " linked-title)))
        (delete-region (org-element-property :begin element)
                       (org-element-property :end element))
	      (insert logseq-link)))))

(defun nm--migrate-denote-file-to-logseq (file)
  "Convert denote note FILE to logseq.
Copy the file from `denote-directory' to `logseq-directory' and
convert all denote links to logseq."
  (let ((dest (expand-file-name
	       (file-name-nondirectory file)
               logseq-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (nm--convert-denote-links-to-logseq)
      (write-file dest))))

(defun nm--ensure-dir-path (path)
  "Ensure that PATH is a directory path.
A path is directory path if it ends with a \"/\". Might not work
on windows."
  (if (string-match-p (rx "/" line-end) path)
      path
    (concat path "/")))

;;;; Apply nm--convert-links to all files
(defun nm--migrate-denote-to-logseq ()
  "Migrate all denote notes logseq.
Migrate all org files in `denote-directory' to logseq notes in
`logseq-directory'."
  (let* ((denote-directory (nm--ensure-dir-path denote-directory))
         (logseq-directory (nm--ensure-dir-path logseq-directory))
         (denote-org-files (directory-files denote-directory t "\.org$")))
    (mapcar #'nm--migrate-denote-file-to-logseq denote-org-files)))
