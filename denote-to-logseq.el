;;; Global variables which are required
(require 'denote)

(defvar logseq-directory nil)

(setq logseq-directory "~/notes/test-logseq/")

;;; Copy contents of DENOTE directory to LOGSEQ directory
(defun nm--copy-org-files-from-to (src dest)
  "Copy all Org files from SRC to DEST.
SRC is supposed to be denote's, DEST logseq's notes directory."
  ;; TODO: Rewrite using mechanisms provided by DENOTE  
  (let ((contents (directory-files src t "org$")))
    (mapcar #'(lambda (file)
		(copy-file file dest t))
	    contents)))

;;; Find a file based on denote ID and get its TITLE
(let* ((denote-directory logseq-directory)
       (tmp-id "20220909T083622")
       (file (denote-get-path-by-id tmp-id)))
  (denote-link--file-type-regexp (denote-retrieve-title-value file 'org)))



(defun nm--convert-links-denote-to-logseq-one-file (file)
  "In FILE, convert all denote links to logseq links.
Use the following steps:
(1) In FILE, look for an expression matching denote-link regexp.
(2) Find the linked-to file using denote's identifier.
(3) Open the linked-to file and get its title.
(4) Using the linked-to file's title, replace the
denote-link-format with logseq's.
(5) REPEAT."
  (let* ((denote-link-rx (rx (and (group "[[denote:" (= 8 num))
				  (group "T" (= 6 num))
				  (group "][" (* (or alnum print)))
				  (group "]]")))))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward denote-link-rx nil t 1)
	(let* ((element (org-element-copy (org-element-context)))
	       (linked-id (org-element-property :path element))
	       (linked-file (denote-get-path-by-id linked-id))
	       (linked-title (denote-retrieve-title-value linked-file 'org))
	       (logseq-link (concat "[[" linked-title "]]")))
	  ;; (re-search-backward denote-link-rx nil t 1)
	  (replace-match logseq-link)))
      (buffer-substring (point-min) (point-max)))))


(nm--convert-links-denote-to-logseq-one-file "~/notes/test-logseq/20221004T094518--kontrollstrukturen-iteration__cl_syntax_fluss.org")

;;; Apply nm--convert-links to all files
(defun nm--convert-links-denote-to-logseq (dir)
  "In DIR, converts all denote links to logseq links."
  (mapcar #'(lambda (file)
	      (nm--convert-links-denote-to-logseq-one-file file))
	  (directory-files dir t (rx "org" line-end))))


;;; Other stuff
(defun test-fn (in-file out-file)
  (with-temp-buffer
    (erase-buffer)
    (insert (org-file-contents in-file))

    (delete-file out-file)
    (write-file out-file nil)))
