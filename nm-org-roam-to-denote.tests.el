(require 'ert)
(require 'org-roam)
(require 'nm-org-roam-to-denote)

(ert-deftest nm--roam-node-denote-filename-standard-test ()
  "Test standard filename generation with a title and tags."
  (let ((node (org-roam-node-create :id "2023102512000000"
                                    :title "My Standard Title"
                                    :tags '("Emacs" "LISP"))))
    (setf (org-roam-node-file node) "/tmp/20231025120000-blabla.org")

    (should (string= (nm--roam-node-denote-filename node)
                     "20231025T120000--my-standard-title__emacs_lisp.org"))))

(ert-deftest nm--roam-node-denote-filename-no-tags-test ()
  "Test filename generation when the org-roam node has no tags.
Ensures strict Denote compliance by omitting the '__' separator."
  (let ((node (org-roam-node-create :id "20231025120000"
                                    :title "No Tags Title"
                                    :tags nil)))
    (setf (org-roam-node-file node) "/tmp/20231025120000-blabla.org")

    (should (string= (nm--roam-node-denote-filename node)
                     "20231025T120000--no-tags-title.org"))))

(ert-deftest nm--roam-node-denote-filename-slashes-in-title-test ()
  "Test that slashes in the title are correctly replaced with hyphens."
  (let ((node (org-roam-node-create :id "20231025120000"
                                    :title "Title/With/Slashes"
                                    :tags '("testing"))))
    (setf (org-roam-node-file node) "/tmp/20231025120000-blabla.org")

    (should (string= (nm--roam-node-denote-filename node)
                     "20231025T120000--title-with-slashes__testing.org"))))
