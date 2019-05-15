.PHONY: docs

docs : racket-mode.info racket-mode.html

racket-mode.texi : racket-mode.org reference.org
	emacs --batch -Q -l ox-texinfo racket-mode.org --eval "(setq indent-tabs-mode nil)" --funcall org-texinfo-export-to-texinfo

reference.org : generate.el
	emacs --batch -Q --eval '(progn (add-to-list (quote load-path) "${PWD}/../") (package-initialize))' -l generate.el  --funcall racket-make-doc

racket-mode.info : racket-mode.texi
	makeinfo --force --no-split $< -o $@

racket-mode.html : racket-mode.texi
	makeinfo --force --html --no-split --no-headers --css-ref='main.css' $<
