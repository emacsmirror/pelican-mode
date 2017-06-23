;;; pelican-mode.el --- Minor mode for editing Pelican sites -*- lexical-binding: t -*-
;;
;; Copyright 2013-2017 Joe Wreschnig
;;
;; Author: Joe Wreschnig <joe.wreschnig@gmail.com>
;; Package-Version: 20170618
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

(require 'subr-x)

(defun pelican-timestamp (&optional time)
  "Generate a Pelican-compatible timestamp for TIME."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun pelican-is-markdown ()
  "Check if the buffer is likely using Markdown."
  (derived-mode-p 'markdown-mode))

(defun pelican-field (name value)
  "Format a line for a field NAME with a VALUE."
  (if value
      (cond ((derived-mode-p 'markdown-mode)
             (format "%s: %s\n" (capitalize name) value))
            ((derived-mode-p 'rst-mode)
             (format ":%s: %s\n" (downcase name) value))
            (t (error "Unsupported major mode %S" major-mode)))
    ""))

(defun pelican-rst-title (title)
  "Create a ReSt version of TITLE."
  (concat title "\n" (make-string (string-width title) ?#) "\n\n"))

(defun pelican-title (title)
  "Format a TITLE for the current document, according to major mode."
  (cond ((derived-mode-p 'markdown-mode)
         (pelican-field "title" title))
        ((derived-mode-p 'rst-mode)
         (pelican-rst-title title))
        (t (error "Unsupported major mode %S" major-mode))))

(defun pelican-header (title date status category tags slug)
  "Create a Pelican header."
  ;; TODO: Use a property list (-> alist via seq-partition) instead.
  (when (eq date t)
    (setq date (pelican-timestamp)))
  
  (concat (pelican-title title)
          (pelican-field "date" date)
          (pelican-field "status" status)
          (pelican-field "tags" tags)
          (pelican-field "category" category)
          (pelican-field "slug" slug)
          "\n"))

(defun pelican-insert-draft-post-header (title tags)
  "Insert a Pelican header for a draft post."
  (interactive "sPost title: \nsTags: ")
  (let ((slug (pelican-default-slug)))
    (save-excursion
      (goto-char 0)
      (insert (pelican-header title 't "draft" nil tags slug)))))

(defun pelican-insert-page-header (title hidden)
  "Insert a Pelican header for a page."
  (interactive
   (list (read-string "Page title: ")
         (y-or-n-p "Hidden? ")))
  (let ((slug (pelican-default-slug))
        (hidden (if hidden "hidden" nil)))
    (save-excursion
      (goto-char 0)
      (insert (pelican-header title nil hidden nil nil slug)))))

(defun pelican-insert-header ()
  "Insert a Pelican header for a page or post."
  (interactive)
  (call-interactively (if (pelican-is-page)
                          'pelican-insert-page-header
                        'pelican-insert-draft-post-header)))

(defun pelican-set-field (field value)
  "Set FIELD to VALUE."
  (interactive "sField: \nsValue: ")
  (save-excursion
    (goto-char 0)
    (if (re-search-forward (concat "^" (pelican-field field ".+*")) nil t)
        (replace-match (pelican-field field value))
      (re-search-forward "#")
      (forward-line 2)
      (re-search-forward "^$")
      (replace-match (pelican-field field value)))))

(defun pelican-set-title (title)
  "Set the title to TITLE."
  (interactive "sTitle: ")
  (if (pelican-is-markdown)
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
  (pelican-set-field "date" (pelican-timestamp)))

(defun pelican-publish-draft ()
  "Remove draft status from a Pelican post."
  (interactive)
  (pelican-set-field "status" nil)
  (pelican-update-date))

(defun pelican-is-page ()
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

(defun pelican-is-in-site ()
  "Check if this buffer is under a Pelican site."
  (not (null (pelican-find-root))))

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (if-let ((default-directory (pelican-find-root)))
      (let ((output (get-buffer-create "*Pelican Output*")))
        (display-buffer output)
        (pop-to-buffer output)
        (compilation-mode)
        (start-process "Pelican Makefile" output "make" target))
    (message "This doesn't look like a Pelican site.")))

(defun pelican-make-html ()
  "Generate HTML via a Makefile at the root of the site."
  (interactive)
  (pelican-make "html"))

(defun pelican-make-rsync-upload ()
  "Upload with rsync via a Makefile at the root of the site."
  (interactive)
  (pelican-make "rsync_upload"))

(defconst pelican-keymap (make-sparse-keymap)
  "The default keymap used in Pelican mode.")
(define-key pelican-keymap (kbd "C-c P n")
  'pelican-insert-header)
(define-key pelican-keymap (kbd "C-c P p")
  'pelican-publish-draft)
(define-key pelican-keymap (kbd "C-c P t")
  'pelican-update-date)
(define-key pelican-keymap (kbd "C-c P h")
  'pelican-make-html)
(define-key pelican-keymap (kbd "C-c P u")
  'pelican-make-rsync-upload)


;;;###autoload
(define-minor-mode pelican-mode
  "Toggle Pelican mode.

Interactively with no argument, this command toggles the mode.
to show buffer size and position in mode-line."
  :init-value nil
  :lighter " Pelican"
  :keymap pelican-keymap
  :group 'pelican)

;;;###autoload
(defun pelican-enable-if-site ()
  "Enable `pelican-mode' if this buffer is under a Pelican site."
  (when (pelican-is-in-site)
    (pelican-mode 1)))

;;;###autoload
(add-hook 'markdown-mode-hook 'pelican-enable-if-site)

;;;###autoload
(add-hook 'rst-mode-hook 'pelican-enable-if-site)

(provide 'pelican-mode)
;;; pelican-mode.el ends here
