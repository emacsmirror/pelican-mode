;;; pelican-mode-test.el --- Tests pelican-mode  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Joe Wreschnig
;;
;; Author: Joe Wreschnig
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
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
;; This file contains test cases for pelican-mode.  Unless you’re
;; hacking on it you shouldn’t need to edit or run this file.



;;; Code:

(require 'ert)
(require 'pelican-mode)

(defun pelican-mode-test-article (mode expected1 expected2)
  "Create an article in MODE and perform some edits.

After the first edits, the buffer should contain EXPECTED1; after
the second, EXPECTED2."
  (with-temp-buffer
    (rename-buffer "pelican-test.rst")
    (funcall mode)
    (pelican-mode)

    (should (not (pelican-mode-find-root)))
    (should (not (pelican-mode-page-p)))
    (should (equal "pelican-test" (pelican-mode-default-slug)))

    (insert "Not really a Pelican article.")
    (pelican-mode-insert-article-header "Testing" "a, b, c")
    (pelican-mode-set-field :test "hello world")
    (pelican-mode-set-field :date "1111-11-11 11:11:11")
    (should (equal (buffer-string) expected1))

    (pelican-mode-publish)
    (pelican-mode-set-fields
     :title "More Tests"
     :date "2222-22-22 22:22:22")
    (pelican-mode-remove-field "test")
    (should (equal (buffer-string) expected2))))

(ert-deftest pelican-mode-test-rst-mode ()
  (pelican-mode-test-article
   #'rst-mode
   "\
Testing
#######

:date: 1111-11-11 11:11:11
:status: draft
:slug: pelican-test
:tags: a, b, c
:test: hello world

Not really a Pelican article."

   "\
More Tests
##########

:date: 2222-22-22 22:22:22
:slug: pelican-test
:tags: a, b, c

Not really a Pelican article."))

(ert-deftest pelican-mode-test-org-mode ()
  (pelican-mode-test-article
   #'org-mode
   "\
#+TITLE: Testing
#+DATE: 1111-11-11 11:11:11
#+PROPERTY: STATUS draft
#+PROPERTY: SLUG pelican-test
#+PROPERTY: TAGS a, b, c
#+PROPERTY: TEST hello world

Not really a Pelican article."

   "\
#+TITLE: More Tests
#+DATE: 2222-22-22 22:22:22
#+PROPERTY: SLUG pelican-test
#+PROPERTY: TAGS a, b, c

Not really a Pelican article."))

;;; pelican-mode-test.el ends here
