# WG21 Org authoring extensions

The exporters use ordinary Org syntax where Org already has a suitable
construct.  This page documents the WG21-specific additions.

## Proposed wording

Paragraphs use `pnum` blocks.  An omitted label or `#` advances the current
paragraph; dotted labels maintain independent nested counts.  Decimal and
literal components pin that part of the path:

```org
#+begin_wording
#+begin_pnum 2
Pinned paragraph.
#+end_pnum
#+begin_pnum #.#
Paragraph 2.1.
#+end_pnum
#+begin_pnum #.#
Paragraph 2.2.
#+end_pnum
#+begin_pnum #
Paragraph 3.
#+end_pnum
#+end_wording
```

For list-shaped wording, opt in with `#+begin_wording :pnums lists`.  Its
top-level ordered items become paragraphs and nested ordered or unordered
items become dotted subparagraphs.  Org counter cookies such as `[@5]` pin a
component.  Use explicit `pnum` blocks for literal labels such as `x` and
`x+1`.

HTML paragraph numbers are self-links.  Within a generated clause whose
stable name is `example.clause`, `[[sref:example.clause/2.1]]` links directly
to paragraph 2.1.

Inline additions and removals use `[[insert:][new text]]` and
`[[delete:][old text]]`.  The shorthand
`[[replace:new text][old text]]` emits the removal followed by the addition.
URL-encode characters that have link syntax in the replacement; for example,
`[[replace:%2Fnew%2F][old]]` italicizes the replacement.
Use `[[mark:][highlighted text]]` for neutral highlighting.  Org markup is
allowed in the displayed text.

### Wording code

`codeblock` and `itemdecl` blocks are literal draft code, not Org
source blocks.  They recognize balanced draft escapes, including nested
escapes and C++ braces:

```org
#+begin_codeblock
void f(@\added{T{1, 2}, @\emph{term}@}@);
@\replace{old<T>{}}{new<T>{}}@
#+end_codeblock
```

The supported commands are `added`, `removed`, `replace`, `mark`, `emph`,
`math`, `sref`, `exposid`, `exposidnc`, `placeholder`, `grammarterm`,
`terminal`, `descr`, `seebelow`, `impdef`, `impdefnc`, `unspec`, and `atsign`.  Bare `\ref{name}`
in code comments is also recognized.  Unclosed arguments of these commands and
missing closing `@` delimiters stop export with a diagnostic.  Any other escape
(another macro, an optional argument, several macros in one escape) passes
through to LaTeX unchanged, while supported escapes nested inside it are still
expanded and have their inner `@` delimiters removed.  HTML export stops with
a diagnostic naming the unmodelled outer escape.  An unknown `@\...` sequence
without a closing `@` is treated as literal code, so C++ escapes such as
`@\u{00E9}` and format strings remain usable.

If a literal `@` shares a line with another `@`, the text is inherently
ambiguous with an escape.  Spell each literal at-sign as `@\atsign@`; this
renders as `@` in both HTML and LaTeX.  For example, author
`"@\atsign@\t{}@\atsign@"` to show the C++ string `"@\t{}@"`.

These escapes do not apply to ordinary `src` blocks.  Those remain literal
Org source blocks suitable for live or transcluded code.

### Grammar

A `grammar` block holds productions written the way the working draft prints
them, and sets them the way it does:

```org
#+begin_grammar
escaped-identifier:
    ` identifier `

operator-function-id:
    ~operator~ operator
    ~typename~_{opt} nested-name-specifier_{opt} type-name

sign: one of
    +  -
#+end_grammar
```

- A line at the left margin ending in `:` names the nonterminal being
  defined; `: one of` gives a "one of" production.
- Each indented line is one alternative.  A blank line starts the next
  production.
- Words, including hyphenated ones, are nonterminals.  Runs of punctuation
  are terminals.
- A terminal that looks like a word, such as a keyword, is written as inline
  code: `~typename~`.  `=typename=` means the same, but `~` is the house
  spelling.  In a "one of" production every token is a terminal, and the
  spacing between them is kept so rows line up.
- `name_{opt}` marks an optional element.
- A line that is only `...` stands for alternatives not shown.
- `[[insert:][…]]`, `[[delete:][…]]`, `[[replace:new][old]]` and
  `[[mark:][…]]` work inside a line, as do the draft escapes above.  Use
  `@\descr{…}@` for prose, such as "any member of the basic character set".

## Notes and examples

Draft notes use `#+begin_note`.  A draft example is spelled
`#+begin_wgexample`, because Org always reads `#+begin_example` as its own
literal block:

```org
#+begin_wgexample
An example.
#+end_wgexample
```

Both are numbered inside wording.  `:number N` pins the number and
`:unnumbered` drops it.

## Converting from mpark/wg21 markdown

Pandoc writes mpark/wg21 constructs as generic Org that does not carry their
meaning.  Rewrite them in the native forms:

- A `bnf` grammar becomes a `#+begin_verse` block.  Use `#+begin_grammar`, and
  mark keywords that pandoc left as plain words, such as `~operator~`.
- Inline code becomes `=code=`, because pandoc's Org writer uses verbatim for
  it.  It exports the same as `~code~`, but rewrite it as `~code~` to match the
  house spelling.
- An example becomes the text `[/Example/: … — /end example/]`, which Org will
  not italicise.  Use `#+begin_wgexample`.

## Export options

Export options may be set with `#+OPTIONS:` or with `#+bind:` and the option's
variable name, one per line:

```org
#+bind: org-export-with-toc nil
#+bind: org-export-with-sub-superscripts nil
```

The `make` export honours `#+bind:`.  The values are read, never evaluated.
An interactive Emacs needs `org-export-allow-bind-keywords` set to `t` to
export such a paper the same way.

## Comparison tables

The compact form declares headings once and takes source blocks as cells,
left-to-right and then top-to-bottom.  Captions and widths are normal affiliated
keywords:

```org
#+caption: Alternative implementations
#+attr_wg21: :columns 60 40
#+begin_cmptbl :headers "Portable | Native"
#+begin_src C++
portable();
#+end_src
#+begin_src C++
native();
#+end_src
#+end_cmptbl
```

There may be any number of columns.  Widths are positive integer percentages
and must total 100.  For cells containing prose or multiple Org elements, use
the explicit `#+begin_cmptblcell before` form demonstrated in `basic.org`;
existing explicit tables remain supported.

## Citations

`wg21.bib` is the default bibliography.  A paper that uses an Org citation such
as `[cite:@P2996R13]` needs no `#+BIBLIOGRAPHY`, References heading, or
`#+PRINT_BIBLIOGRAPHY`; those are still honored when supplied.  Refresh the
index with `make wg21.bib` after removing the old file.

Use `[[cite-title:P2996R13]]` for the mpark-style form that includes the paper
title.  Export warns when a cited P-paper revision is older than the newest
revision in the local index, when the revision is omitted, or when its series
is absent from the index.

## Code highlighting

Unlabelled source blocks default to C++, so the usual paper needs no language
option.  A document can change that default and teach Emacs font-lock about
proposed C++ keywords:

```org
#+WG21_CODE_LANGUAGE: C++
#+WG21_CPP_KEYWORDS: inspect reflexpr
```

An explicit language overrides the document default.  Use `text` (or
`fundamental`) for an unhighlighted block.  Inline `~code~` stays plain
monospace.  Use native Org inline source when highlighting is useful:

```org
A plain name is ~vector~; a C++ declaration is src_cpp{int value;}.
```

Inline source exports its code without evaluation by default, including in
headings.  Normal Org properties and element parameters override that default
when evaluation or results are intentional.  Org source blocks are literal;
they do not enable embedded prose markup.  Proposed wording blocks retain
their separate, unhighlighted added/removed markup rules.

## Paper metadata

For `#+DOCNUMBER: P2996R13`, and for the pre-publication spelling
`#+DOCNUMBER: D2996R13`, HTML metadata automatically includes `Latest` and
`Status` links for the P2996 paper series.  Placeholders and N-paper numbers do
not get those links.

## Slides

A talk is an Org file named `talk.slides.org`, written like a paper, and
`make talk.slides.html` exports it as a reveal.js presentation; `make slides`
builds every talk.  The exporter is org-re-reveal, so its keywords and
attributes work as usual, and the WG21 blocks work as they do in a paper:
wording with paragraph numbers and `insert:`/`delete:`/`replace:` links,
`note` and `wgexample` blocks, comparison tables, grammar, code coloured by
the same face stylesheets, citations, and math.

```org
#+title: Optional References
#+subtitle: An update for LEWG
#+author: A. Author
#+email: author@example.com
#+DOCNUMBER: P1234R2
#+AUDIENCE: LEWG

#+begin_notes
Speaker notes for the title slide.
#+end_notes

* Motivation
** The problem
#+ATTR_REVEAL: :frag (appear)
- One point at a time
- Then the next
#+begin_notes
Speaker notes; press s for the speaker view.
#+end_notes
```

- A level-one heading starts a section, and a level-two heading starts a slide
  within it.  `#+OPTIONS: H:1` makes every heading a slide.
- The title slide shows the title and subtitle, then the document number,
  date, audience, and reply-to address.  `#+REVEAL_TITLE_SLIDE:` replaces it.
- `#+begin_notes` is speaker notes; `#+begin_note` is a WG21 note.
- The defaults are the `white` theme and the notes, search, and zoom plugins;
  set `#+REVEAL_THEME:` and `#+REVEAL_PLUGINS:` to change them.  Only the
  white, black, and contrast themes embed their fonts, so `make check` reports
  any other.
- Like a paper, a talk is one self-contained file.  reveal.js itself is not in
  this repository: the Makefile fetches the release it pins into
  `.deps/reveal.js`, checks it, and the exporter embeds it.

## Live preview

`make serve` builds the papers and talks, serves them at
<http://127.0.0.1:8000>, and rebuilds them whenever a source changes, files an
Org file links to included.  A page open in the browser reloads itself when it
is rebuilt, keeping its scroll position or its slide.  When a build fails the
page keeps the last good version and shows the build log over it.

The other goals name what to build and serve:

```sh
make serve                       # everything
make serve basic.html            # one paper; / opens it
make serve talk.slides.html      # one talk
make serve slides                # every talk
```

A PDF is rebuilt and served, but does not reload; use a viewer that reloads the
file itself.  The reload script is added as a page is served, never written
into the built file.  Set `SERVE_HOST` or `SERVE_PORT` on the command line, or
in a `local.mk` beside the Makefile, which is not committed.
