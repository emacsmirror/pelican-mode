pelican-mode is an Emacs minor mode for editing pages and posts in
[Pelican] sites.

It's intended to be used alongside [markdown-mode] or [rst-mode]. It
also assumes you've set up Pelican with `pelican-quickstart` or
something like it. In particular it assumes:

 * The existence of `pelicanconf.py` and `Makefile` in some ancestor
   directory.
 * The first component of the path (e.g. `content`) after that
   ancestor is irrelevant.
 * If the next component is `pages`, that indicates a static page
   rather than a dated post.
   
It also enforces some parts of my preferred Pelican configuration:

 * Categories are never provided (you can have one if you want, but
   the default interactive commands don't provide one).
 * Tags are always provided.
 * Slugs are explicit, and include nested subdirectories.

## Quick Guide

* `C-x p n` - Insert a post or page header
* `C-x p p` - Remove draft status from a post (i.e. publish it)
* `C-x p t` - Update the date field in a post/page header
* `C-x p h` - Generate HTML output for a site (equivalent to `make html`)
* `C-x p u` - Upload a site using rsync (equivalent to `make rsync_upload`)

## Troubleshooting

If the commands which invoke `make` can find the Makefile but can't
find `pelican`, your `exec-path` may not be set right. Try out
[exec-path-from-shell].

## License

This code is released into the public domain via the
[CC0 Public Domain Dedication][0].

 [Pelican]: http://getpelican.com/
 [markdown-mode]: http://jblevins.org/projects/markdown-mode/
 [rst-mode]: http://docutils.sourceforge.net/docs/user/emacs.html
 [exec-path-from-shell]: https://github.com/purcell/exec-path-from-shell
 [0]: http://creativecommons.org/publicdomain/zero/1.0/legalcode
