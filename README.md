pelican-mode is an Emacs minor mode for editing articles and pages in
[Pelican][] sites.

It's intended to be used alongside a major mode for the Pelican
document. Currently supported formats are Markdown, reStructuredText,
AsciiDoc, and Org. It also assumes you've set up Pelican with
`pelican-quickstart` or something like it. In particular it expects:

 * The existence of `pelicanconf.py` and `Makefile` in some ancestor
   directory.
 * The first component of the path (e.g. `content`) after that
   ancestor is irrelevant.
 * If the next component is `pages`, that indicates a static page
   rather than a dated post.


## Quick Guide

To enable by default on all text files in a Pelican site:

``` emacs-lisp
(require 'pelican-mode)
(pelican-global-mode)
```


Or with `use-package` and deferred loading:

``` emacs-lisp
(use-package pelican-mode
  :after (:any org rst markdown-mode adoc-mode)
  :config
  (pelican-global-mode))
```

Or, register `pelican-mode` or `pelican-mode-enable-if-site`
as hook functions for more direct control.

* `C-c = d` - Update the document’s date
* `C-c = f` - Set a metadata field (title, category, etc.)
* `C-c = h` - Generate HTML output for a site (equivalent to `make html`)
* `C-c = n` - Insert a post or page header
* `C-c = p` - Remove draft status from a post (i.e. publish it)
* `C-c = u` - Upload a site using rsync (equivalent to `make rsync_upload`)


## Troubleshooting

If the commands which invoke `make` can find the Makefile but can't
find `pelican`, your `exec-path` may not be set right. Try out
[exec-path-from-shell][].


## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

 [Pelican]: http://getpelican.com/
 [markdown-mode]: http://jblevins.org/projects/markdown-mode/
 [rst-mode]: http://docutils.sourceforge.net/docs/user/emacs.html
 [exec-path-from-shell]: https://github.com/purcell/exec-path-from-shell
