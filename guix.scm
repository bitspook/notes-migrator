(use-modules
 (guix packages)
 (guix gexp)
 (guix utils)
 (guix git-download)
 (guix licenses)
 (guix build-system emacs)
 (gnu packages base)
 (gnu packages emacs)
 (gnu packages emacs-xyz))

(package
  (name "emacs-notes-migrator")
  (version "0.1.0-dev")
  (source
   (local-file
    "./" "nm-checkout"
    #:recursive? #t
    #:select? (git-predicate (current-source-directory))))
  (native-inputs (list gnu-make emacs-minimal))
  (propagated-inputs (list emacs-org-roam emacs-denote))
  (build-system emacs-build-system)
  (synopsis "Migrate notes.")
  (description "Migrate notes.")
  (home-page "https://github.com/bitspook/notes-migrator")
  (license gpl3+))
