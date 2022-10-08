;;; Global variables which are required
(defvar my-denote-directory nil)
(defvar my-logseq-directory nil)

(setq my-denote-directory "~/notes/test-denote/")
(setq my-logseq-directory "~/notes/test-logseq/")

;;; Copy contents of DENOTE directory to LOGSEQ directory
(defun nm--copy-org-files-from-to (src dest)
  "Copy all Org files from SRC to DEST.
SRC is supposed to be denote's, DEST logseq's notes directory."
  (let ((contents (directory-files src t "org$")))
    (mapcar #'(lambda (file)
		(copy-file file dest t))
	    contents)))

;;; Find a file based on denote ID
(with-temp-buffer
  (insert-file (car (directory-files my-logseq-directory t "20221007T210006")))
  (buffer-substring (point-min) (point-max)))

;;; Get an Org mode file's TITLE
(with-temp-buffer
  (insert-file (car (directory-files my-logseq-directory t "20221006T215838")))
  (org-mode)
  (let* ((key (org-collect-keywords '("TITLE")))
	 (string (cadar key)))
    string))

(defun nm--convert-links-denote-to-logseq-one-file (file)
  "In FILE, convert all denote links to logseq links.
(1) In FILE, look for an expression matching denote-link
regexp.  (2) Find the linked-to file using denote's
identifier.  (3) Open the linked-to file and get its title.
(4) Using the linked-to file's title, replace the
denote-link-format with logseq's.  REPEAT"

  
  
  )


;;; Apply nm--convert-links to all files
(defun nm--convert-links-denote-to-logseq (dir)
  "In DIR, converts all denote links to logseq links."
  (mapcar #'(lambda (file)
	      (nm--convert-links-denote-to-logseq-one-file file))
	  (directory-files dir t (rx "org" line-end))))
;;; Replace denote-link with logseq-link, first try
(setq one-file (car (directory-files my-denote-directory t "org$")))

(defun nm--convert-denote-links-to-logseq (filename)
  (let* ((denote-link-rx (rx (and (group "[[denote:" (= 8 num))
				  (group "T" (= 6 num))
				  (group "][" (* (or alnum print)))
				  (group "]]")))))
    (with-temp-buffer
      (insert-file filename)
      (while (re-search-forward denote-link-rx nil t)
	  (let* ((element (org-element-copy (org-element-context)))
		 ;; (parsed-buffer (org-element-parse-buffer))
		 (linked-id (org-element-property :path element))
		 (linked-file (car (directory-files my-denote-directory t linked-id)))
		 (linked-title (with-temp-buffer
				 (insert-file linked-file)
				 (org-mode)
				 (cadar (org-collect-keywords '("TITLE")))))
		 (logseq-format (concat "[[" linked-title "]]")))
	    (replace-match logseq-format)))
      (buffer-substring (point-min) (point-max)))))

(nm--convert-denote-links-to-logseq one-file)

;;; Other stuff
(defun nm--convert-denote-links-to-logseq (filename)
  (let ((denote-link-rx (rx (and (group "[[denote:" (= 8 num))
				 (group	"T" (= 6 num))
				 (group "][" (* (or ascii nonascii)))
				 (group	"]]")))))
    (while (re-search-forward denote-link-rx nil t)
      (let* (())))))



(defun test-fn (in-file out-file)
  (with-temp-buffer
    (erase-buffer)
    (insert (org-file-contents in-file))

    (delete-file out-file)
    (write-file out-file nil)))
