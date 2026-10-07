EMACS ?= emacs
PACKAGE_LINT_DIR ?= .package-lint

.PHONY: check compile test checkdoc lint clean
check: compile test checkdoc
compile:
	$(EMACS) --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile guildai.el
test:
	$(EMACS) --batch -Q -L . -l test/guildai-test.el -f ert-run-tests-batch-and-exit
checkdoc:
	$(EMACS) --batch -Q --eval '(progn (require (quote checkdoc)) (checkdoc-file "guildai.el"))'
clean:
	rm -f *.elc

lint:
	$(EMACS) --batch -Q -L $(PACKAGE_LINT_DIR) -l package-lint --eval '(with-temp-buffer (insert-file-contents "guildai.el") (emacs-lisp-mode) (let ((issues (package-lint-buffer))) (prin1 issues) (when issues (kill-emacs 1))))'
