# Vendored Emacs packages

`org-transclusion` is an upstream-tracking Git submodule. It is pinned to the
exact revision recorded by this repository because the source-line features
used by papers have historically landed upstream before a package release.

Initialize the pinned revision after cloning:

```sh
git submodule update --init packages/org-transclusion
```

To inspect a newer upstream `main` without losing the recorded provenance:

```sh
git submodule update --remote packages/org-transclusion
git -C packages/org-transclusion log --oneline --decorate -n 10
```

Review and test that update, then commit the changed gitlink in this repository.
The submodule's upstream is <https://github.com/nobiot/org-transclusion>.
