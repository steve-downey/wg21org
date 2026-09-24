DEPS_DIR := .deps
EMACS := $(shell command -v emacs 2> /dev/null)

# Evaluate org-babel code blocks when exporting: yes or no.
BABEL ?= yes

PAPERS := $(wildcard *.org)
HTML := $(PAPERS:.org=.html)

EXPORT_HTML = WG21_BABEL=$(BABEL) $(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--eval '(setq enable-local-variables :all)' \
	--load ox-wg21html.el \
	--visit $< -f my-wg21-export-to-html -f wg21org-exit

.DELETE_ON_ERROR:

.PHONY: all
all: $(HTML)

wg21.bib:
	curl https://wg21.link/index.bib > wg21.bib

%.pdf : %.tex wg21.bib | $(VENV)
	mkdir -p $(DEPS_DIR)
	$(SOURCE_VENV) latexmk -shell-escape -pdflua -use-make -deps -deps-out=$(DEPS_DIR)/$@.d -MP $<

%.html: %.org ox-wg21html.el wg21-links.el emacs.d/export-init.el
	$(EXPORT_HTML)

# Export every paper, keep going past failures, and report them all.
.PHONY: check
check:
	@failed=; \
	for org in $(PAPERS); do \
	  html=$${org%.org}.html; \
	  if $(MAKE) --no-print-directory -B $$html > $(DEPS_DIR)/$$html.log 2>&1; \
	  then echo "ok      $$html"; \
	  else echo "FAILED  $$html  (see $(DEPS_DIR)/$$html.log)"; failed="$$failed $$html"; fi; \
	done; \
	test -z "$$failed"
check: | $(DEPS_DIR)

$(DEPS_DIR):
	mkdir -p $@

.PHONY: test
test:
	$(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--load test/ox-wg21html-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	latexmk -c

# Include dependencies
$(foreach file,$(TARGET),$(eval -include $(DEPS_DIR)/$(file).d))
