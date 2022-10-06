;;;; Migrating notes from DENOTE to LOGSEQ format (including links)
;;; Global variables which are required
(defvar my-denote-directory nil)
(defvar my-logseq-directory nil)

(setq my-denote-directory "~/notes/test-denote/")
(setq my-logseq-directory "~/notes/test-logseq/")
;;; Copy contents of DENOTE directory to LOGSEQ directory
(defun nm--copy-files-from-to (from to)
  (let ((contents (directory-files from t "org$")))
    (mapcar #'(lambda (file)
		(copy-file file to t))
	    contents)))

;;; Find a file based on denote ID
(with-temp-buffer
  (insert-file (car (directory-files my-logseq-directory t "20221006T215838")))
  (buffer-substring (point-min) (point-max)))

;;; Replace buffer contents via regexp
(setq one-file (cadr (directory-files my-denote-directory t "org$")))


(let ((denote-link-rx (rx (and (group "[[denote:" (= 8 num))
			       (group	"T" (= 6 num))
			       (group "][" (* (or ascii nonascii)))
			       (group	"]]"))))
      (logseq-link (concat "[[" "sexy schmexy" "]]")))
  (with-temp-buffer
    (insert-file one-file)
    (replace-regexp denote-link-rx logseq-link)
    (buffer-substring (point-min) (point-max))))

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

(denote "many title" '("asdf") nil (car denote-dired-directories) nil nil)

;; (replace-regexp (rx (and (group "[[denote:" (= 8 num))
;; 			 (group	"T" (= 6 num))
;; 			 (group "][" (* (or ascii nonascii)))
;; 			 (group	"]]")))
;; 		"asdf")

;; [[denote:20220911T091111][my description]]
