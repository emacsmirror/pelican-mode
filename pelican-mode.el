;;; pelican-mode.el --- Minor mode for editing pages and posts in Pelican sites
;;
;; Author: Joe Wreschnig
;; This code is released into the public domain.

;;; Commentary:
;;
;; Probably, this doesn't handle a lot of error cases.  I also never
;; tested it on networked drives and the lookup for pelicanconf.py
;; might slow it down considerably.

;;; Code:

(defun pelican-timestamp-now ()
  "Generate a Pelican-compatible timestamp."
  (format-time-string "%Y-%m-%d %H:%M"))

(defun pelican-is-markdown ()
  "Check if the buffer is likely using Markdown."
  (eq major-mode 'markdown-mode))

(defun pelican-field (name value)
  "Helper to format a field NAME and VALUE."
  (if value (format "%s: %s\n" name value) ""))

(defun pelican-markdown-header (title date status category tags slug)
  "Generate a Pelican Markdown header.

All parameters but TITLE may be nil to omit them. DATE may be a
string or 't to use the current date and time."
  (let ((title (format "Title: %s\n" title))
        (status (pelican-field "Status" status))
        (category (pelican-field "Category" category))
        (tags (pelican-field "Tags" tags))
        (slug (pelican-field "Slug" slug))
        (date (if date (format "Date: %s\n"
                               (if (stringp date) date
                                 (pelican-timestamp-now)))
                "")))
    (concat title date status tags category slug "\n")))

(defun pelican-rst-header (title date status category tags slug)
  "Generate a Pelican reStructuredText header.

All parameters but TITLE may be nil to omit them. DATE may be a
string or 't to use the current date and time."
  (let ((title (format "%s\n%s\n\n" title
                       (make-string (string-width title) ?#)))
        (status (pelican-field ":status" status))
        (category (pelican-field ":category" category))
        (tags (pelican-field ":tags" tags))
        (slug (pelican-field ":slug" slug))
        (date (if date (format ":date: %s\n"
                               (if (stringp date) date
                                 (pelican-timestamp-now)))
                "")))
    (concat title date status tags category slug "\n")))

(defun pelican-insert-draft-post-header (title tags)
  "Insert a Pelican header for a draft post."
  (interactive "sPost title: \nsTags: ")
  (let ((slug (pelican-default-slug))
        (header (if (pelican-is-markdown)
                    'pelican-markdown-header 'pelican-rst-header)))
    (save-excursion
      (goto-char 0)
      (insert (funcall header title 't "draft" nil tags slug)))))

(defun pelican-insert-page-header (title hidden)
  "Insert a Pelican header for a page."
  (interactive
   (list (read-string "Page title: ")
         (y-or-n-p "Hidden? ")))
  (let ((slug (pelican-default-slug))
        (hidden (if hidden "hidden" nil))
        (header (if (pelican-is-markdown)
                    'pelican-markdown-header 'pelican-rst-header)))
    (save-excursion
      (goto-char 0)
      (insert (funcall header title nil hidden nil nil slug)))))

(defun pelican-insert-header ()
  "Insert a Pelican header for a page or post."
  (interactive)
  (call-interactively (if (pelican-is-page)
                          'pelican-insert-page-header
                        'pelican-insert-draft-post-header)))

(defun pelican-update-date ()
  "Update a Pelican date header."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let* ((field (if (pelican-is-markdown) "Date" ":date"))
           (re (format "^%s: [-0-9 :]+\n" field))
           (date (pelican-timestamp-now)))
      (if (re-search-forward re nil t)
          (replace-match (format "%s: %s\n" field date))
        (message "This doesn't look like a Pelican page.")))))

(defun pelican-publish-draft ()
  "Remove draft status from a Pelican post."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let* ((field (if (pelican-is-markdown) "Status" ":status"))
           (re (format "^%s: draft\n" field)))
      (if (re-search-forward re nil t)
          (progn
            (replace-match (format ""))
            (pelican-update-date))
        (message "This doesn't look like a Pelican draft.")))))

(defun pelican-is-page ()
  "Guess the current buffer is a Pelican page (vs. a post or neither)."
  (let ((pelican-base (pelican-find-root)))
    (if pelican-base
        (let* ((relative (file-relative-name buffer-file-name pelican-base))
               (components (split-string relative "/")))
          (string= "pages" (car (cdr components)))))))

(defun pelican-default-slug ()
  "Generate a Pelican post/page slug for the current buffer."
  (let ((pelican-base (pelican-find-root))
        (file-name (file-name-sans-extension buffer-file-name)))
    (if pelican-base
        (let* ((relative (file-relative-name file-name pelican-base))
               (components (cdr (split-string relative "/")))
               (components (if (string= "pages" (car components))
                               (cdr components) components)))
          (mapconcat 'identity components "/"))
      (format "%s/%s"
              (file-name-nondirectory
               (directory-file-name
                (file-name-directory file-name)))
              (file-name-base file-name)))))

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
  (let ((conf (pelican-find-in-parents "pelicanconf.py")))
    (if conf (file-name-directory conf))))

(defun pelican-is-in-site ()
  "Check if this buffer is under a Pelican site."
  (not (not (pelican-find-root))))

(defun pelican-enable-if-site ()
  "Enable `pelican-mode' if this buffer is under a Pelican site."
  (if (pelican-is-in-site)
      (pelican-mode 1)))

(defun pelican-make (target)
  "Execute TARGET in a Makefile at the root of the site."
  (interactive "sMake Pelican target: ")
  (let ((default-directory (pelican-find-root)))
    (if default-directory
        (let ((output (get-buffer-create "*Pelican Output*")))
          (display-buffer output)
          (pop-to-buffer output)
          (compilation-mode)
          (start-process "Pelican Makefile" output "make" target))
      (message "This doesn't look like a Pelican site."))))

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

(define-minor-mode pelican-mode
  "Toggle Pelican mode.

Interactively with no argument, this command toggles the mode.
to show buffer size and position in mode-line.
"
  :init-value nil
  :lighter " Pelican"
  :keymap pelican-keymap
  :group 'pelican
  )

(add-hook 'markdown-mode-hook 'pelican-enable-if-site)
(add-hook 'rst-mode-hook 'pelican-enable-if-site)

(provide 'pelican-mode)

;;; pelican-mode.el ends here
