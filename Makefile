clean:
	@rm -f init.elc mikemacs.el mikemacs.elc

compile: init.el mikemacs.org clean
	@emacs -Q --batch -l 'lisp/compile.el'

update: 
	@emacs -Q --batch -l "lisp/update.el"
