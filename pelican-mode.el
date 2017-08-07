;;; pelican-mode.el --- Minor mode for editing Pelican sites -*- lexical-binding: t -*-
;;
;; Copyright 2013-2017 Joe Wreschnig
;;
;; Author: Joe Wreschnig <joe.wreschnig@gmail.com>
;; Package-Version: 20170730
;; Package-Requires: ((emacs "25"))
;; Keywords: convenience, editing
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; pelican-mode is an Emacs minor mode for editing pages and posts in
;; Pelican sites.  Pelican is a static site generator which can
;; process a variety of text file formats.  For more information, see
;; URL https://blog.getpelican.com/.
;;
;; It's intended to be used alongside `markdown-mode' or `rst-mode'.
;; It also assumes you've set up Pelican with ``pelican-quickstart''
;; or something like it.  In particular it assumes:
;;
;;  * The existence of ``pelicanconf.py'' and ``Makefile'' in some
;;    ancestor directory.
;;  * The first component of the path (e.g. ``content'') after that
;;    ancestor is irrelevant.
;;  * If the next component is ``pages'', that indicates a page
;;    rather than an article.


;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup pelican-mode nil
  "Support for Pelican articles and pages.

For more information about Pelican see URL https://blog.getpelican.com/."
  :group 'convenience)

(defcustom pelican-mode-default-page-fields
  '(:slug slug)
  "Fields to include when creating a new page.

See the documentation for `pelican-mode-set-field' for more information
about metadata fields and special values."
  :group 'pelican-mode
  :type '(plist))

(defcustom pelican-mode-default-article-fields
  '(:date now :status "draft" :slug slug)
  "Fields to include when creating a new article.

See the documentation for `pelican-mode-set-field' for more information
about metadata fields and special values."
  :group 'pelican-mode
  :type '(plist))

(defcustom pelican-mode-formats
  '((markdown-mode . pelican-mode-set-field-markdown-mode)
    (adoc-mode . pelican-mode-set-field-adoc-mode)
    (rst-mode . pelican-mode-set-field-rst-mode))
  "Functions to handle setting metadata, based on major mode.

This association list maps modes to functions that take two
arguments, field and value strings."
  :group 'pelican-mode
  :type '(alist :key-type function :value-type function))

(defun pelican-mode-timestamp (&optional time)
  "Generate a pelican-mode-compatible timestamp for TIME."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun pelican-mode-insert-header (&rest fields)
  "Insert a Pelican header for an article with metadata FIELDS."
  (mapc (apply-partially #'apply #'pelican-mode-set-field)
        (seq-partition fields 2)))

(defun pelican-mode-insert-draft-article-header (title tags)
  "Insert a Pelican header for a draft with a TITLE and TAGS."
  (interactive "sArticle title: \nsTags: ")
  (apply #'pelican-mode-insert-header
         `(:title ,title ,@pelican-mode-default-article-fields :tags ,tags)))

(defun pelican-mode-insert-page-header (title &optional hidden)
  "Insert a Pelican header for a page with a TITLE, potentially HIDDEN."
  (interactive
   (list (read-string "Page title: ")
         (y-or-n-p "Hidden? ")))
  (apply #'pelican-mode-insert-header
         `(:title ,title ,@pelican-mode-default-page-fields
                  :hidden ,(when hidden "hidden"))))

(defun pelican-mode-insert-auto-header ()
  "Insert a Pelican header for a page or article."
  (interactive)
  (call-interactively
   (if (pelican-mode-page-p)
       #'pelican-mode-insert-page-header
     #'pelican-mode-insert-draft-article-header)))

(defun pelican-mode-set-field-rst-mode (field value)
  "Set reStructuredText metadata FIELD to VALUE."
  (setq field (downcase field))
  (if (equal field "title")
      (let ((header (format "%s\n%s\n\n"
                            value (make-string (string-width value) ?#))))
        (if (looking-at ".*\n#+\n+")
            (replace-match header)
          (insert header)))
    (let ((text (when value (format ":%s: %s\n" field value))))
      (when (looking-at "^.*\n#")
        (forward-line 3))
      (if (re-search-forward (format "^:%s:.*\n" (regexp-quote field)) nil t)
          (replace-match (or text ""))
        (when text
          (if (re-search-forward "^$" nil t)
              (replace-match text)
            (insert text)))))))

(defun pelican-mode-set-field-markdown-mode (field value)
  "Set Markdown metadata FIELD to VALUE."
  (setq field (capitalize field))
  (let ((text (when value (format "%s: %s\n" field value))))
    (if (re-search-forward (format "^%s:.*\n" (regexp-quote field)) nil t)
        (replace-match text)
      (when value
        (if (re-search-forward "^$" nil t)
            (replace-match text)
          (insert text))))))

(defun pelican-mode-set-field-adoc-mode (field value)
  "Set AsciiDoc metadata FIELD to VALUE."
  (setq field (downcase field))
  (if (equal field "title")
      (let ((header (format "= %s\n\n" value)))
        (if (looking-at "= .*\n\n+")
            (replace-match header)
          (insert header)))
    (let ((text (when value (format ":%s: %s\n" field value))))
      (when (looking-at "^=")
        (forward-line 2))
      (if (re-search-forward (format "^:%s:.*\n" (regexp-quote field)) nil t)
          (replace-match (or text ""))
        (when text
          (if (re-search-forward "^$" nil t)
              (replace-match text)
            (insert text)))))))

(defun pelican-mode-set-field (field value)
  "Set FIELD to VALUE.

FIELD may be a string or a symbol; if it is a symbol, the
symbol name is used (removing a leading ':' if present).

VALUE may be any value; except for the following special values,
the unquoted printed representation of it is used:

- `now' means the current time; see `pelican-mode-timestamp'.

- `slug' means the file's path relative to the document root sans
  extension; see `pelican-mode-default-slug'.

- nil or an empty string removes the field."
  (interactive "sField: \nsValue: ")
  (setq value (pcase value
                ('now (pelican-mode-timestamp))
                ('slug (pelican-mode-default-slug))
                ('"" nil)
                (_ value)))
  (when (symbolp field)
    (setq field (string-remove-prefix ":" (symbol-name field))))
  (let ((set-field
         (assoc-default nil pelican-mode-formats #'derived-mode-p)))
    (unless set-field
      (error "Unsupported major mode %S" major-mode))
    (save-excursion
      (goto-char 0)
      (funcall set-field field value))))

(defun pelican-mode-remove-field (field)
  "Remove FIELD."
  (pelican-mode-set-field field nil))

(defun pelican-mode-set-title (title)
  "Set the title to TITLE."
  (interactive "sTitle: ")
  (pelican-mode-set-field :title title))

(defun pelican-mode-update-date ()
  "Update a Pelican date header."
  (interactive)
  (pelican-mode-set-field :date 'now))

(defun pelican-mode-publish-draft ()
  "Remove draft status from a Pelican article."
  (interactive)
  (pelican-mode-remove-field :status)
  (pelican-mode-update-date))

(defun pelican-mode-page-p ()
  "Return non-nil the current buffer is a Pelican page."
  (when-let (pelican-mode-base (pelican-mode-find-root))
    (let* ((relative (file-relative-name buffer-file-name pelican-mode-base))
           (components (split-string relative "/")))
      (equal "pages" (cadr components)))))

(defun pelican-mode-default-slug ()
  "Generate a Pelican article/page slug for the current buffer."
  (if-let ((pelican-mode-base (pelican-mode-find-root))
           (file-name (file-name-sans-extension buffer-file-name)))
      (let* ((relative (file-relative-name file-name pelican-mode-base))
             (components (cdr (split-string relative "/")))
             (components (if (string= "pages" (car components))
                             (cdr components) components)))
        (mapconcat 'identity components "/"))
    (when-let (file-name (file-name-sans-extension buffer-file-name))
      (file-name-base file-name))))

(defun pelican-mode-find-root ()
  "Return the root of the buffer's Pelican site, or nil."
  (locate-dominating-file default-directory "pelicanconf.py"))

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (if-let (default-directory (pelican-mode-find-root))
      (compilation-start (format "make %s" target)
                         nil (lambda (_) "*pelican*"))
    (user-error "No Pelican site root could be found")))

(defun pelican-make-html ()
  "Generate HTML via a Makefile at the root of the site."
  (interactive)
  (pelican-make "html"))

(defun pelican-make-rsync-upload ()
  "Upload with rsync via a Makefile at the root of the site."
  (interactive)
  (pelican-make "rsync_upload"))

;;;###autoload
(define-minor-mode pelican-mode
  "Toggle Pelican mode.
With a prefix argument ARG, enable Pelican mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Pelican is a static site generator which can process a variety of
text file formats.  For more information, see URL
https://blog.getpelican.com/.

When Pelican mode is enabled, additional commands are available
for editing articles or pages:

\\{pelican-mode-map}"
  :lighter " Pelican"
  :keymap `((,(kbd "C-c P n") . pelican-mode-insert-auto-header)
            (,(kbd "C-c P p") . pelican-mode-publish-draft)
            (,(kbd "C-c P t") . pelican-mode-update-date)
            (,(kbd "C-c P h") . pelican-make-html)
            (,(kbd "C-c P u") . pelican-make-rsync-upload)))

;;;###autoload
(define-minor-mode pelican-global-mode
  "Toggle Pelican global mode.
With a prefix argument ARG, enable Pelican global mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Pelican is a static site generator which can process a variety of
text file formats.  For more information, see URL
https://blog.getpelican.com/.

When Pelican global mode is enabled, text files which seem to
be part of a Pelican site will have `pelican-mode' automatically
enabled.

If you disable this, you may still enable `pelican-mode' manually
or add `pelican-mode-enable-if-site' to more specific mode
hooks."
  :global t
  :group 'pelican-mode
  (if pelican-global-mode
      (add-hook 'text-mode-hook #'pelican-mode-enable-if-site)
    (remove-hook 'text-mode-hook #'pelican-mode-enable-if-site)))

;;;###autoload
(defun pelican-mode-enable-if-site ()
  "Enable `pelican-mode' if this buffer is part of a Pelican site."
  (when (pelican-mode-find-root)
    (pelican-mode 1)))

(provide 'pelican-mode)
;;; pelican-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
