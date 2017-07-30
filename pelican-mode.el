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
  "Support for Pelican posts and pages."
  :group 'convenience)

(defcustom pelican-mode-default-page-fields
  '(:slug slug)
  "Fields to include when creating a new page.

See the documentation for `pelican-field' for more information
about metadata fields and special values."
  :group 'pelican-mode
  :type '(plist))

(defcustom pelican-mode-default-post-fields
  '(:date now :status "draft" :slug slug)
  "Fields to include when creating a new post.

See the documentation for `pelican-field' for more information
about metadata fields and special values."
  :group 'pelican-mode
  :type '(plist))

(defun pelican-timestamp (&optional time)
  "Generate a Pelican-compatible timestamp for TIME."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun pelican-field (name value)
  "Format a line for a field NAME with a VALUE.

NAME may be a string or a symbol; if it is a symbol, the
symbol name is used (removing a leading ':' if present).

VALUE may be any value; except for the following special values,
the unquoted printed representation of it is used:

- `now' means the current time; see `pelican-timestamp'.

- `slug' means the file's path relative to the document root sans
  extension; see `pelican-default-slug'.

- nil or an empty strings means return an empty string, without
  any name or value."
  (setq value (pcase value
                ('now (pelican-timestamp))
                ('slug (pelican-default-slug))
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

(defun pelican-rst-title (title)
  "Format a reStructureText version of TITLE."
  (concat title "\n" (make-string (string-width title) ?#) "\n\n"))

(defun pelican-title (title)
  "Format a TITLE for the current document, according to major mode."
  (cond ((derived-mode-p 'markdown-mode)
         (pelican-field "title" title))
        ((derived-mode-p 'rst-mode)
         (pelican-rst-title title))
        (t (error "Unsupported major mode %S" major-mode))))

(defun pelican-header (title &rest fields)
  "Generate a Pelican header for a post with a TITLE and metadata FIELDS."
  (concat (pelican-title title)
          (mapconcat (apply-partially #'apply #'pelican-field)
                     (seq-partition fields 2) "")
          "\n"))

(defun pelican-insert-header (title &rest fields)
  "Insert a Pelican header for a post with a TITLE and metadata FIELDS."
  (save-excursion
    (goto-char 0)
    (insert (apply #'pelican-header (cons title fields)))))

(defun pelican-insert-draft-post-header (title tags)
  "Insert a Pelican header for a draft with a TITLE and TAGS."
  (interactive "sPost title: \nsTags: ")
  (apply #'pelican-insert-header
         `(,title ,@pelican-mode-default-post-fields :tags ,tags)))

(defun pelican-insert-page-header (title &optional hidden)
  "Insert a Pelican header for a page with a TITLE, potentially HIDDEN."
  (interactive
   (list (read-string "Page title: ")
         (y-or-n-p "Hidden? ")))
  (apply #'pelican-insert-header
         `(,title ,@pelican-mode-default-page-fields
                  :hidden ,(when hidden "hidden"))))

(defun pelican-insert-auto-header ()
  "Insert a Pelican header for a page or post."
  (interactive)
  (call-interactively
   (if (pelican-page-p)
       #'pelican-insert-page-header
     #'pelican-insert-draft-post-header)))

(defun pelican-set-field (field value)
  "Set FIELD to VALUE."
  (interactive "sField: \nsValue: ")
  (save-excursion
    (goto-char 0)
    (when (and (derived-mode-p 'rst-mode)
               (re-search-forward "^#" nil t))
      (forward-line 2))
    (if (re-search-forward (concat "^" (pelican-field field ".+*")) nil t)
        (replace-match (pelican-field field value))
      (when value
        (re-search-forward "^$")
        (replace-match (pelican-field field value))))))

(defun pelican-remove-field (field)
  "Remove FIELD."
  (pelican-set-field field nil))

(defun pelican-set-title (title)
  "Set the title to TITLE."
  (interactive "sTitle: ")
  (if (derived-mode-p 'markdown-mode)
      (pelican-set-field "title" title)
    (save-excursion
      (goto-char 0)
      (let ((header (pelican-rst-title title)))
        (if (looking-at ".*\n#+\n+")
            (replace-match header)
          (insert header))))))

(defun pelican-update-date ()
  "Update a Pelican date header."
  (interactive)
  (pelican-set-field :date 'now))

(defun pelican-publish-draft ()
  "Remove draft status from a Pelican post."
  (interactive)
  (pelican-remove-field :status)
  (pelican-update-date))

(defun pelican-page-p ()
  "Guess the current buffer is a Pelican page (vs. a post or neither)."
  (when-let (pelican-base (pelican-find-root))
    (let* ((relative (file-relative-name buffer-file-name pelican-base))
           (components (split-string relative "/")))
      (equal "pages" (cadr components)))))

(defun pelican-default-slug ()
  "Generate a Pelican post/page slug for the current buffer."
  (if-let ((pelican-base (pelican-find-root))
           (file-name (file-name-sans-extension buffer-file-name)))
      (let* ((relative (file-relative-name file-name pelican-base))
             (components (cdr (split-string relative "/")))
             (components (if (string= "pages" (car components))
                             (cdr components) components)))
        (mapconcat 'identity components "/"))
    (format "%s/%s"
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory file-name)))
            (file-name-base file-name))))

(defun pelican-find-in-parents (file-name)
  "Find FILE-NAME in the default directory or one of its parents, or nil."
  (let* ((parent (expand-file-name default-directory)))
    (while (and (not (file-readable-p (concat parent file-name)))
                (not (string= parent (directory-file-name parent))))
      (setq parent (file-name-directory (directory-file-name parent))))
    (let ((found (concat parent file-name)))
      (if (file-readable-p found) found nil))))

(defun pelican-find-root ()
  "Return the root of the buffer's Pelican site, or nil."
  (when-let (conf (pelican-find-in-parents "pelicanconf.py"))
    (file-name-directory conf)))

(defun pelican-site-p ()
  "Check if this buffer is under a Pelican site."
  (not (null (pelican-find-root))))

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (if-let (default-directory (pelican-find-root))
      (compilation-start (format "make %s" target)
                         nil (lambda (_) "*pelican*"))
    (message "This doesn't look like a Pelican site.")))

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

Interactively with no argument, this command toggles the mode.
for editing Pelican site files."
  :lighter " Pelican"
  :group 'pelican
  :keymap `((,(kbd "C-c P n") . pelican-insert-auto-header)
            (,(kbd "C-c P p") . pelican-publish-draft)
            (,(kbd "C-c P t") . pelican-update-date)
            (,(kbd "C-c P h") . pelican-make-html)
            (,(kbd "C-c P u") . pelican-make-rsync-upload)))

;;;###autoload
(defun pelican-enable-if-site ()
  "Enable `pelican-mode' if this buffer is under a Pelican site."
  (when (pelican-site-p)
    (pelican-mode 1)))

;;;###autoload
(add-hook 'markdown-mode-hook 'pelican-enable-if-site)

;;;###autoload
(add-hook 'rst-mode-hook 'pelican-enable-if-site)

(provide 'pelican-mode)
;;; pelican-mode.el ends here
