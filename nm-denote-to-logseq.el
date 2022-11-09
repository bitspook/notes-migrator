;; fasdf
;;; nm-denote-to-logseq.el -- Utilities to migrate denote notes to logseq
;;
;; Author: hapst3r
;; Version: 0.1
;; URL: https://github.com/bitspook/notes-migrator/denote-to-logseq.el
;; Keywords: Denote, Logseq
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (denote "1.0.0")
;;
;;; Commentary:
;; Migrate Denote (org-mode only) notes to Logseq.
;;
;;; Code:
(require 'denote)

(defvar logseq-directory nil)

(defun nm--convert-denote-links-to-logseq ()
  "Convert all denote links to logseq links in current buffer.
Current-buffer should be in `org-mode'."
  (let ((denote-link-rx (rx "[[denote:")))
    (while (re-search-forward denote-link-rx nil t)
      (when-let* ((element (org-element-copy (org-element-context)))
		  (link-desc (buffer-substring-no-properties
			      (org-element-property :contents-begin element)
			      (org-element-property :contents-end element)))
		  (linked-id (org-element-property :path element))
                  (linked-file (denote-get-path-by-id linked-id))
                  (linked-title (denote-retrieve-title-value linked-file 'org))
                  (logseq-link (if (string= link-desc linked-title)
				   (format "[[%s]]" linked-title)
				 (format "[[%s][%s]]" linked-title link-desc))))
        (replace-region-contents
         (org-element-property :begin element)
         (org-element-property :end element)
         (lambda () (format "%s " logseq-link)))))))

(defun nm--convert-denote-tags-to-logseq (file)
  "Convert denote tags header to logseq tags header in file."
  (let* ((tags (denote-retrieve-keywords-value file 'org))
	 (tags-joined (string-join tags ", "))
	 (denote-tags-rx (rx "#+filetags:"))
	 (logseq-tags-string (concat "#+tags:" tags-joined)))
    (when (re-search-forward denote-tags-rx nil t)
      (let ((element (org-element-copy (org-element-context))))
	(replace-region-contents
	 (org-element-property :begin element)
	 (org-element-property :end element)
	 (lambda () (format "%s" logseq-tags-string)))))))

(defun nm--migrate-denote-file-to-logseq (file)
  "Convert denote note FILE to logseq.
Copy the file from `denote-directory' to `logseq-directory' and
convert all denote links to logseq."
  (let* ((denote-silo (file-name-directory file))
         (logseq-silo
          (concat
           logseq-directory
           (string-remove-prefix denote-directory
                                 denote-silo)))
         (dest
          (expand-file-name
           (file-name-nondirectory file)
           logseq-silo)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (nm--convert-denote-tags-to-logseq file)
      (nm--convert-denote-links-to-logseq)
      (unless (file-directory-p logseq-silo)
        (make-directory logseq-silo t))
      (write-file dest))))

(defun nm-migrate-denote-to-logseq ()
  "Migrate all denote notes logseq.
Migrate all org files in `denote-directory' to logseq notes in
`logseq-directory'."
  (interactive)
  (let ((denote-org-files (directory-files-recursively denote-directory "\.org$")))
    (mapcar #'nm--migrate-denote-file-to-logseq denote-org-files)))

(provide 'nm-denote-to-logseq)
;;; nm-denote-to-logseq.el ends here
