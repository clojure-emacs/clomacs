version:
	emacs --version

test : version
	cask exec emacs --script test/elisp/clomacs-test.el

elpa:
	cask install
	cask update
