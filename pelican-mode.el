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
;; Probably, this doesn't handle a lot of error cases.  I also never
;; tested it on networked drives and the lookup for pelicanconf.py
;; might slow it down considerably.


;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup pelican-mode nil
  "Support for Pelican articles and pages."
  :group 'convenience)

(defcustom pelican-mode-default-page-fields
  '(:slug slug)
  "Fields to include when creating a new page.

See the documentation for `pelican-mode-field' for more information
about metadata fields and special values."
  :group 'pelican
  :type '(plist))

(defcustom pelican-mode-default-article-fields
  '(:date now :status "draft" :slug slug)
  "Fields to include when creating a new article.

See the documentation for `pelican-mode-field' for more information
about metadata fields and special values."
  :group 'pelican
  :type '(plist))

(defun pelican-mode-timestamp (&optional time)
  "Generate a pelican-mode-compatible timestamp for TIME."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun pelican-mode-field (name value)
  "Format a line for a field NAME with a VALUE.

NAME may be a string or a symbol; if it is a symbol, the
symbol name is used (removing a leading ':' if present).

VALUE may be any value; except for the following special values,
the unquoted printed representation of it is used:

- `now' means the current time; see `pelican-mode-timestamp'.

- `slug' means the file's path relative to the document root sans
  extension; see `pelican-mode-default-slug'.

- nil or an empty strings means return an empty string, without
  any name or value."
  (setq value (pcase value
                ('now (pelican-mode-timestamp))
                ('slug (pelican-mode-default-slug))
                ('"" nil)
                (_ value)))
  (when (symbolp name)
    (setq name (string-remove-prefix ":" (symbol-name name))))
  (if value
      (cond ((derived-mode-p 'markdown-mode)
             (format "%s: %s\n" (capitalize name) value))
            ((derived-mode-p 'rst-mode)
             (format ":%s: %s\n" (downcase name) value))
            (t (error "Unsupported major mode %S" major-mode)))
    ""))

(defun pelican-mode-rst-title (title)
  "Format a reStructureText version of TITLE."
  (concat title "\n" (make-string (string-width title) ?#) "\n\n"))

(defun pelican-mode-title (title)
  "Format a TITLE for the current document, according to major mode."
  (cond ((derived-mode-p 'markdown-mode)
         (pelican-mode-field "title" title))
        ((derived-mode-p 'rst-mode)
         (pelican-mode-rst-title title))
        (t (error "Unsupported major mode %S" major-mode))))

(defun pelican-mode-header (title &rest fields)
  "Generate a Pelican header for an article with a TITLE and metadata FIELDS."
  (concat (pelican-mode-title title)
          (mapconcat (apply-partially #'apply #'pelican-mode-field)
                     (seq-partition fields 2) "")
          "\n"))

(defun pelican-mode-insert-header (title &rest fields)
  "Insert a Pelican header for an article with a TITLE and metadata FIELDS."
  (save-excursion
    (goto-char 0)
    (insert (apply #'pelican-mode-header (cons title fields)))))

(defun pelican-mode-insert-draft-article-header (title tags)
  "Insert a Pelican header for a draft with a TITLE and TAGS."
  (interactive "sArticle title: \nsTags: ")
  (apply #'pelican-mode-insert-header
         `(,title ,@pelican-mode-default-article-fields :tags ,tags)))

(defun pelican-mode-insert-page-header (title &optional hidden)
  "Insert a Pelican header for a page with a TITLE, potentially HIDDEN."
  (interactive
   (list (read-string "Page title: ")
         (y-or-n-p "Hidden? ")))
  (apply #'pelican-mode-insert-header
         `(,title ,@pelican-mode-default-page-fields
                  :hidden ,(when hidden "hidden"))))

(defun pelican-mode-insert-auto-header ()
  "Insert a Pelican header for a page or article."
  (interactive)
  (call-interactively
   (if (pelican-mode-page-p)
       #'pelican-mode-insert-page-header
     #'pelican-mode-insert-draft-article-header)))

(defun pelican-mode-set-field (field value)
  "Set FIELD to VALUE."
  (interactive "sField: \nsValue: ")
  (save-excursion
    (goto-char 0)
    (when (and (derived-mode-p 'rst-mode)
               (re-search-forward "^#" nil t))
      (forward-line 2))
    (if (re-search-forward (concat "^" (pelican-mode-field field ".+*")) nil t)
        (replace-match (pelican-mode-field field value))
      (when value
        (re-search-forward "^$")
        (replace-match (pelican-mode-field field value))))))

(defun pelican-mode-remove-field (field)
  "Remove FIELD."
  (pelican-mode-set-field field nil))

(defun pelican-mode-set-title (title)
  "Set the title to TITLE."
  (interactive "sTitle: ")
  (if (derived-mode-p 'markdown-mode)
      (pelican-mode-set-field "title" title)
    (save-excursion
      (goto-char 0)
      (let ((header (pelican-mode-rst-title title)))
        (if (looking-at ".*\n#+\n+")
            (replace-match header)
          (insert header))))))

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
    (format "%s/%s"
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory file-name)))
            (file-name-base file-name))))

(defun pelican-mode-find-in-parents (file-name)
  "Find FILE-NAME in the default directory or one of its parents, or nil."
  (let* ((parent (expand-file-name default-directory)))
    (while (and (not (file-readable-p (concat parent file-name)))
                (not (string= parent (directory-file-name parent))))
      (setq parent (file-name-directory (directory-file-name parent))))
    (let ((found (concat parent file-name)))
      (if (file-readable-p found) found nil))))

(defun pelican-mode-find-root ()
  "Return the root of the buffer's Pelican site, or nil."
  (when-let (conf (pelican-mode-find-in-parents "pelicanconf.py"))
    (file-name-directory conf)))

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (if-let (default-directory (pelican-mode-find-root))
      (compilation-start (format "make %s" target)
                         nil (lambda (_) "*pelican*"))
    (user-error "This doesn't look like a Pelican site")))

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

When Pelican global mode is enabled, text files which seem to
be part of a Pelican site will have `pelican-mode' automatically
enabled.

If you disable this, you may still enable `pelican-mode' manually
or add `pelican-mode-enable-if-site' to more specific mode
hooks."
  :global t
  :group 'pelican
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
