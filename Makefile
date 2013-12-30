temp:
	mkdir -p temp

temp/company-mode: temp
	git clone https://github.com/company-mode/company-mode temp/company-mode

temp/tern: temp
	git clone https://github.com/marijnh/tern temp/tern
	cd temp/tern && npm install --production

.PHONY: deps
deps: temp/tern temp/company-mode

emacs:
	emacs -Q -L $(PWD) -L temp/company-mode -L temp/tern/emacs -l script/init.el

.PHONY: clean
clean:
	rm -rf temp
