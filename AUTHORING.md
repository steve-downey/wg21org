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

### Wording code and grammar

`codeblock`, `itemdecl`, and `grammar` blocks are literal draft code, not Org
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
`terminal`, `seebelow`, `impdef`, `impdefnc`, `unspec`, and `atsign`.  Bare `\ref{name}`
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
