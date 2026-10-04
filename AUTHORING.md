# WG21 Org authoring extensions

The exporters use ordinary Org syntax where Org already has a suitable
construct.  This page documents the WG21-specific additions.

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

Source blocks continue to use Org's normal language name.  A document can fill
in the language of otherwise-unlabelled blocks and teach Emacs font-lock about
proposed C++ keywords:

```org
#+WG21_CODE_LANGUAGE: C++
#+WG21_CPP_KEYWORDS: inspect reflexpr
```

An explicit language overrides the document default.  Use `text` (or
`fundamental`) for an unhighlighted block.  Org source blocks are literal by
default; they do not enable embedded prose markup.  Proposed wording blocks
retain their separate added/removed markup rules.

## Paper metadata

For `#+DOCNUMBER: P2996R13`, and for the pre-publication spelling
`#+DOCNUMBER: D2996R13`, HTML metadata automatically includes `Latest` and
`Status` links for the P2996 paper series.  Placeholders and N-paper numbers do
not get those links.
