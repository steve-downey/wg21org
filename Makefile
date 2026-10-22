DEPS_DIR := .deps
EMACS := $(shell command -v emacs 2> /dev/null)
PYTHON ?= python3

# Settings for this checkout only, such as SERVE_PORT.
-include local.mk

# Evaluate org-babel code blocks when exporting: yes or no.
BABEL ?= yes

# A talk is talk.slides.org, and becomes talk.slides.html.
SLIDES := $(wildcard *.slides.org)
PAPERS := $(filter-out $(SLIDES),$(wildcard *.org))
HTML := $(PAPERS:.org=.html)
SLIDES_HTML := $(SLIDES:.org=.html)
ORG_TRANSCLUSION := packages/org-transclusion
ORG_TRANSCLUSION_SOURCES := $(ORG_TRANSCLUSION)/org-transclusion.el \
	$(ORG_TRANSCLUSION)/org-transclusion-src-lines.el \
	$(ORG_TRANSCLUSION)/text-clone.el

TRANSCODE_WORDING := wording/wording.org

EXPORT_HTML = WG21_BABEL=$(BABEL) $(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--eval '(setq enable-local-variables :all)' \
	--load ox-wg21html.el \
	--visit $< -f my-wg21-export-to-html -f wg21org-exit

.DELETE_ON_ERROR:

# Keep the .tex a .pdf is made from; make would delete it as intermediate.
.SECONDARY: $(PAPERS:.org=.tex)

.PHONY: all
all: $(HTML) $(SLIDES_HTML)

.PHONY: papers
papers: $(HTML)

.PHONY: slides
slides: $(SLIDES_HTML)

wg21.bib:
	curl https://wg21.link/index.bib > wg21.bib

# The engine is the one the paper names in #+LATEX_COMPILER, which Org
# records in the .tex.
LATEXMK_ENGINE = $$(sed -n 's/^% Intended LaTeX compiler: \(pdf\)\{0,1\}\(.*\)latex$$/-\2\1latex/p' $< | sed 's/^-pdflatex$$/-pdf/')

%.pdf : %.tex wg21.bib | $(VENV)
	mkdir -p $(DEPS_DIR)
	$(SOURCE_VENV) latexmk -interaction=nonstopmode -halt-on-error -shell-escape \
	  $(LATEXMK_ENGINE) -use-make -deps -deps-out=$(DEPS_DIR)/$@.d -MP $<

%.html: %.org ox-wg21html.el wg21-links.el wg21-git.el wg21-cmptbl.el wg21-cite.el wg21-wording.el wg21-grammar.el wg21-front.el wg21-code.el wg21org.css wg21org.el emacs.d/export-init.el $(ORG_TRANSCLUSION_SOURCES)
	$(EXPORT_HTML)

transcode-view.html: $(TRANSCODE_WORDING) transcode-view.bib wg21.bib

# reveal.js, pinned and checked, is fetched only to build a talk.  The
# exporter embeds it, so the talk does not need it afterwards.
REVEAL_JS_VERSION := 6.0.2
REVEAL_JS_SHA256 := 2f0c20bfb02447a198a1ebe8a8c097a5437d035caa38f2820bf4ba887a3eac91
REVEAL_JS := $(DEPS_DIR)/reveal.js
REVEAL_JS_STAMP := $(REVEAL_JS)/.version-$(REVEAL_JS_VERSION)

$(REVEAL_JS_STAMP): | $(DEPS_DIR)
	rm -rf $(REVEAL_JS) $(DEPS_DIR)/reveal.js.tgz
	curl -fsSL -o $(DEPS_DIR)/reveal.js.tgz \
	  https://registry.npmjs.org/reveal.js/-/reveal.js-$(REVEAL_JS_VERSION).tgz
	echo "$(REVEAL_JS_SHA256)  $(DEPS_DIR)/reveal.js.tgz" | sha256sum -c --quiet
	mkdir -p $(REVEAL_JS)
	tar -xzf $(DEPS_DIR)/reveal.js.tgz -C $(REVEAL_JS) --strip-components=1
	rm $(DEPS_DIR)/reveal.js.tgz
	touch $@

EXPORT_SLIDES = WG21_BABEL=$(BABEL) $(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--eval '(setq enable-local-variables :all)' \
	--load ox-wg21reveal.el \
	--visit $< -f wg21-reveal-export-to-html -f wg21org-exit

%.slides.html: %.slides.org ox-wg21reveal.el ox-wg21html.el wg21-links.el wg21-git.el wg21-cmptbl.el wg21-cite.el wg21-wording.el wg21-grammar.el wg21-front.el wg21-code.el wg21reveal.css wg21org.el emacs.d/export-init.el $(ORG_TRANSCLUSION_SOURCES) $(REVEAL_JS_STAMP)
	$(EXPORT_SLIDES)

EXPORT_LATEX = WG21_BABEL=$(BABEL) $(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--eval '(setq enable-local-variables :all)' \
	--load ox-wg21latex.el \
	--visit $< -f my-wg21-export-to-latex -f wg21org-exit

%.tex: %.org ox-wg21latex.el wg21-links.el wg21-git.el wg21-cmptbl.el wg21-cite.el wg21-wording.el wg21-grammar.el wg21-front.el wg21-code.el wg21org-preamble.tex wg21org.el emacs.d/export-init.el $(ORG_TRANSCLUSION_SOURCES)
	$(EXPORT_LATEX)

transcode-view.tex: $(TRANSCODE_WORDING) transcode-view.bib wg21.bib

# A paper must be one self-contained file: anything it would load from
# elsewhere breaks once it is uploaded, or once the other server changes.
# That includes an image beside the paper, which the exporter embeds; an
# <img> whose src is not a data: URI was not.  A talk is held to the same
# rule, so it can be presented with no network.
NOT_DATA := ([^d"'"'"']|d[^a]|da[^t]|dat[^a]|data[^:])
EXTERNAL := <link[^>]*stylesheet|src=.?(https?:)?//|@import|url\(["'"'"']?$(NOT_DATA)|<img[^>]*src="$(NOT_DATA)

# The text a check reads: a talk's embedded scripts are reveal.js, whose
# code mentions URLs it never loads, so the check reads around them.
CHECKED = perl -0pe 's/<script>.*?<\/script>//gs' $(1)

# Export every paper and talk, keep going past failures, and report them all.
.PHONY: check
check:
	@failed=; \
	for org in $(PAPERS) $(SLIDES); do \
	  html=$${org%.org}.html; \
	  if ! $(MAKE) --no-print-directory -W $$org $$html > $(DEPS_DIR)/$$html.log 2>&1; \
	  then echo "FAILED  $$html  (see $(DEPS_DIR)/$$html.log)"; failed="$$failed $$html"; \
	  elif $(call CHECKED,$$html) | grep -Eqi '$(EXTERNAL)'; \
	  then echo "FAILED  $$html  loads external resources:"; \
	       $(call CHECKED,$$html) | grep -Eoi '$(EXTERNAL)[^>]{0,80}' | sed 's/^/          /'; \
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

# Serve papers and talks while they are written, rebuilding them as their
# sources change; a page in the browser reloads when it is rebuilt.  The
# other goals on the command line are what is built and served:
#   make serve                    every paper and talk
#   make serve basic.html         one paper, which / opens
#   make serve slides             every talk
# Set SERVE_HOST and SERVE_PORT in local.mk or on the command line.
SERVE_HOST ?= 127.0.0.1
SERVE_PORT ?= 8000
SERVE_TARGETS := $(filter-out serve,$(MAKECMDGOALS))

.PHONY: serve
serve:
	$(PYTHON) wg21-serve.py --host $(SERVE_HOST) --port $(SERVE_PORT) \
	  --make "$(MAKE)" $(or $(SERVE_TARGETS),all)

# The server builds the other goals itself, and runs until stopped, so
# make has nothing left to do for them.
ifneq ($(filter serve,$(MAKECMDGOALS)),)
$(SERVE_TARGETS): serve
	@:
endif

.PHONY: test
test:
	$(EMACS) --batch --init-directory=emacs.d \
	--load emacs.d/export-init.el \
	--load test/org-transclusion-test.el \
	--load test/ox-wg21html-test.el \
	--load test/ox-wg21latex-test.el \
	--load test/ox-wg21reveal-test.el \
	--load test/wg21org-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	latexmk -c

# Include dependencies
$(foreach file,$(TARGET),$(eval -include $(DEPS_DIR)/$(file).d))
