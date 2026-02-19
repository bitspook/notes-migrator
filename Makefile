test:
	emacs -Q -l nm-org-roam-to-denote.el -batch -l ert -l nm-org-roam-to-denote.tests.el -f ert-run-tests-batch-and-exit
