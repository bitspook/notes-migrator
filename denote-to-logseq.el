;;; Global variables


;;; Copy all files from DENOTE-DIRECTORY to LOGSEQ-DIRECTORY


;;; In LOGSEQ-DIRECTORY, convert denote links to logseq linksh

;;;; List elements in LOGSEQ-DIRECTORY

;;;; For each link: check for an element in listed elements

;;;; Get TITLE of said element
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

(denote "many title" '("asdf") nil (car denote-dired-directories) nil nil)
