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

# Keep the .tex a .pdf is made from; make would delete it as intermediate.
.SECONDARY: $(PAPERS:.org=.tex)

.PHONY: all
all: $(HTML)

wg21.bib:
	curl https://wg21.link/index.bib > wg21.bib

# The engine is the one the paper names in #+LATEX_COMPILER, which Org
# records in the .tex.
LATEXMK_ENGINE = $$(sed -n 's/^% Intended LaTeX compiler: \(pdf\)\{0,1\}\(.*\)latex$$/-\2\1latex/p' $< | sed 's/^-pdflatex$$/-pdf/')

%.pdf : %.tex wg21.bib | $(VENV)
	mkdir -p $(DEPS_DIR)
	$(SOURCE_VENV) latexmk -interaction=nonstopmode -halt-on-error -shell-escape \
	  $(LATEXMK_ENGINE) -use-make -deps -deps-out=$(DEPS_DIR)/$@.d -MP $<

%.html: %.org ox-wg21html.el wg21-links.el wg21-git.el wg21-cmptbl.el wg21-cite.el wg21-wording.el wg21-front.el wg21org.css emacs.d/export-init.el
	$(EXPORT_HTML)

EXPORT_LATEX = WG21_BABEL=$(BABEL) $(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--eval '(setq enable-local-variables :all)' \
	--load ox-wg21latex.el \
	--visit $< -f my-wg21-export-to-latex -f wg21org-exit

%.tex: %.org ox-wg21latex.el wg21-links.el wg21-git.el wg21-cmptbl.el wg21-cite.el wg21-wording.el wg21-front.el wg21org-preamble.tex emacs.d/export-init.el
	$(EXPORT_LATEX)

# A paper must be one self-contained file: anything it would load from
# elsewhere breaks once it is uploaded, or once the other server changes.
# That includes an image beside the paper, which the exporter embeds; an
# <img> whose src is not a data: URI was not.
EXTERNAL := <link[^>]*stylesheet|src=.?(https?:)?//|@import|url\(|<img[^>]*src="([^d"]|d[^a]|da[^t]|dat[^a]|data[^:])

# Export every paper, keep going past failures, and report them all.
.PHONY: check
check:
	@failed=; \
	for org in $(PAPERS); do \
	  html=$${org%.org}.html; \
	  if ! $(MAKE) --no-print-directory -B $$html > $(DEPS_DIR)/$$html.log 2>&1; \
	  then echo "FAILED  $$html  (see $(DEPS_DIR)/$$html.log)"; failed="$$failed $$html"; \
	  elif grep -Eqi '$(EXTERNAL)' $$html; \
	  then echo "FAILED  $$html  loads external resources:"; \
	       grep -Eoi '$(EXTERNAL)[^>]{0,80}' $$html | sed 's/^/          /'; \
	       failed="$$failed $$html"; \
	  else echo "ok      $$html"; fi; \
	done; \
	test -z "$$failed"
check: | $(DEPS_DIR)

# Export every paper to PDF, keep going past failures, and report them all.
.PHONY: check-pdf
check-pdf: | $(DEPS_DIR)
	@failed=; \
	for org in $(PAPERS); do \
	  pdf=$${org%.org}.pdf; \
	  if $(MAKE) --no-print-directory -B $$pdf > $(DEPS_DIR)/$$pdf.log 2>&1; \
	  then echo "ok      $$pdf"; \
	  else echo "FAILED  $$pdf  (see $(DEPS_DIR)/$$pdf.log)"; failed="$$failed $$pdf"; fi; \
	done; \
	test -z "$$failed"

$(DEPS_DIR):
	mkdir -p $@

.PHONY: test
test:
	$(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--load test/ox-wg21html-test.el \
	--load test/ox-wg21latex-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	latexmk -c

# Include dependencies
$(foreach file,$(TARGET),$(eval -include $(DEPS_DIR)/$(file).d))
