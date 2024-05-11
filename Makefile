DEPS_DIR := .deps

wg21.bib:
	curl https://wg21.link/index.bib > wg21.bib

%.pdf : %.tex wg21.bib | $(VENV)
	mkdir -p $(DEPS_DIR)
	$(SOURCE_VENV) latexmk -shell-escape -pdflua -use-make -deps -deps-out=$(DEPS_DIR)/$@.d -MP $<

.PHONY: clean
clean:
	latexmk -c

basic.html: basic.org
	emacs --init-directory=emacs.d/ --batch --load emacs.d/init.el -f package-initialize --eval '(setq enable-local-variables :all)' --load ox-wg21html.el --visit basic.org -f my-wg21-export-to-html

# Include dependencies
$(foreach file,$(TARGET),$(eval -include $(DEPS_DIR)/$(file).d))
