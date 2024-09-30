;;; nm-org-brain-to-denote.el --- Migrate notes from org-brain to denote

;; Copyright (C) 2023  Marcel Arpogaus

;; Author: Marcel Arpogaus
;; Package-Requires: ((emacs "28.1") (denote "3.0.0") (org-brain "0.94"))

;;; Commentary:
;; Migrate org-brain notes to denote. It does not make any changes to
;; org-brain notes, but migrated notes are saved in `denote-directory',
;; while overwriting any conflicting files.

;;; Code:

(require 'denote)
(require 'org-brain)
(require 'files)

(defun nm-org-brain--file-ctime (file)
  "Get creation time of an org-brain FILE.
Use the file's modification time."
  (file-attribute-modification-time (file-attributes file)))

(defun nm-org-brain--file-org-date (file)
  "Extract the date from the org file's DATE attribute.
Return nil if the date is not found."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "#\\+date: +\\([0-9-]+\\)" nil t)
        (date-to-time (concat (match-string 1) "T00:00:00"))
      nil)))

(defun nm-org-brain--file-denote-id (file)
  "Create denote identifier for org-brain FILE.
It returns the timestamp found in the org file's DATE attribute,
or the creation timestamp of FILE if the DATE attribute is not found."
  (format-time-string denote-id-format
                      (or (nm-org-brain--file-org-date file)
                          (nm-org-brain--file-ctime file))))

(defun nm-org-brain--file-denote-filename (file title tags id &optional keep-subdirs)
  "Return valid denote file name for org-brain FILE.
TITLE is the title of the note.
TAGS is a list of strings to be added as tags.
ID is the denote ID of the note.
KEEP-SUBDIRS, if non-nil, retains the relative directory structure."
  (let* ((subdir (and keep-subdirs
                      (file-name-directory
                       (file-relative-name file org-brain-path)))))
    (concat (file-name-as-directory (or subdir ""))
            id "--" (denote-sluggify-title title) "__" (string-join tags "_") ".org")))

(defun nm-org-brain--copy-referenced-file (file dest-dir)
  "Copy FILE to DEST-DIR, creating DEST-DIR if necessary.
Return the new path of the file."
  (let ((dest-file (expand-file-name (file-name-nondirectory file) dest-dir)))
    (make-directory dest-dir t)
    (copy-file file dest-file t)
    dest-file))

(defun nm-org-brain--adjust-file-links (content old-file new-file)
  "Adjust file links in CONTENT to point to the \"files\" subdirectory.
Copy referenced files to the 'files' subdirectory within the denote directory.
Return the adjusted content."
  (let* ((files-dir (file-name-as-directory
                     (expand-file-name "files" (file-name-directory new-file)))))
    (replace-regexp-in-string
     "\\(?:file:\\)\\([[:alnum:]_/.-]+\\)"
     (lambda (match)
       (let ((file (expand-file-name (match-string 1 match)
                                     (file-name-directory old-file))))
         (if (file-regular-p file)
             (format "file:%s" (nm-org-brain--copy-referenced-file file files-dir))
           (format "file:%s" file))))
     content)))

(defun nm-org-brain--convert-brain-links-to-denote (content file)
  "Convert all org-brain links in CONTENT to denote links.
If org-brain entry for a link is not found, a warning is logged and the link is not converted.
Return the converted content."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link-element)
        (when (string-equal (org-element-property :type link-element) "brain")
          (let* ((link-path (org-element-property :path link-element))
                 (target-file (org-brain-entry-path (car (split-string link-path "::" t)))))
            (if-let ((target-entry (or (org-brain-entry-from-id link-path)
                                       (org-brain-get-entry-from-title link-path))))
                (let ((new-link (denote-format-link
                                 target-file
                                 (org-brain-title target-entry)
                                 'org nil)))
                  (replace-string (org-element-property :raw-link link-element)
                                  new-link nil
                                  (org-element-property :begin link-element)
                                  (org-element-property :end link-element)))
              (warn "Failed to convert org-brain link to denote: Could not find target entry for link '%s' in file '%s'" link-path file)))))))
    (buffer-string)))

(defun nm-org-brain--migrate-file (file &optional extra-tags keep-subdirs convert-brain-links adjust-file-links)
  "Convert org-brain FILE to denote note.
Behavior:
- Creates a new file in `denote-directory', preserving subdirectories if KEEP-SUBDIRS is non-nil.
- If file with the name already exists, it is overwritten.
- If CONVERT-BRAIN-LINKS is non-nil, convert org-brain links to denote links.
- If ADJUST-FILE-LINKS is non-nil, adjust file links to point to the 'files' subdirectory and copy the referenced files."
  (let* ((id (nm-org-brain--file-denote-id file))
         (title (org-brain-title (org-brain-path-entry-name file)))
         (tags (mapcar #'downcase (org-brain-get-tags (org-brain-path-entry-name file))))
         (all-tags (if extra-tags
                       (seq-concatenate 'list tags extra-tags)
                     tags))
         (new-name (expand-file-name (nm-org-brain--file-denote-filename file title all-tags id keep-subdirs)
                                     denote-directory))
         (content (org-file-contents file)))
    (message "Migrating '%s' to '%s'..." file new-name)
    (with-temp-buffer
      (erase-buffer)
      (when convert-brain-links
        (setq content (nm-org-brain--convert-brain-links-to-denote content file)))
      (when adjust-file-links
        (setq content (nm-org-brain--adjust-file-links content file new-name)))
      (insert content)
      (org-mode)
      (delete-file new-name)
      (write-file new-name nil)
      (if (denote--edit-front-matter-p new-name 'org)
          (progn (message (format "rewriting front-matter of file %s" new-name))
                 (denote-rewrite-front-matter new-name title all-tags 'org))
        (denote--add-front-matter new-name title all-tags id 'org))
      (save-buffer))))

;;;###autoload
(defun nm-migrate-org-brain-to-denote (&optional extra-tags keep-subdirs convert-brain-links adjust-file-links)
  "Migrate all org-brain notes to denote.
Denote notes are saved as new files in `denote-directory'. Denote must be loaded and configured beforehand. EXTRA-TAGS is a list of strings to be added as tags to all converted files.
CONVERT-BRAIN-LINKS, if non-nil, will convert internal org-brain links to denote links.
ADJUST-FILE-LINKS, if non-nil, will adjust file links to point to the 'files' subdirectory and copy the referenced files there.
When called interactively, prompt for EXTRA-TAGS, KEEP-SUBDIRS, CONVERT-BRAIN-LINKS, and ADJUST-FILE-LINKS."
  (interactive
   (list (denote-keywords-prompt "Add extra TAGS (empty to ignore)")
         (y-or-n-p "Keep subdirectories?")
         (y-or-n-p "Convert org-brain links to denote links?")
         (y-or-n-p "Adjust file links to point to 'files' subdirectory?")))
  (let ((files (org-brain-files))
        (failed-files '()))
    (dolist (file files)
      (condition-case err
          (progn
            (nm-org-brain--migrate-file file extra-tags keep-subdirs convert-brain-links adjust-file-links)
            (message "Successfully migrated '%s'" file))
        (error (push file failed-files)
               (message "Failed to migrate '%s': %s" file err))))
    (message "Migration complete. Failed files: %s" failed-files)))

(provide 'nm-org-brain-to-denote)

;;; nm-org-brain-to-denote.el ends here
