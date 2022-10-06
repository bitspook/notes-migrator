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
