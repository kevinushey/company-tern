;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Version: 0.0.1
;; Package-Requires: ((company "0.8.0") (tern "0.0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'company)
(require 'tern)
(eval-when-compile (require 'cl))

(defun company-tern-prefix ()
  "Grab prefix for tern."
  (and tern-mode
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (tern-run-query
   (lambda (data)
     (funcall callback
              (mapcar (lambda (completion)
                        (let ((candidate (cdr (assq 'name completion))))
                          (put-text-property 0 1 'type (cdr (assq 'type completion)) candidate)
                          (put-text-property 0 1 'doc (cdr (assq 'doc completion)) candidate)
                          candidate))
                      (cdr (assq 'completions data)))))
   '((type . "completions")
     (includeKeywords . t)
     (types . t)
     (docs .t))
   (point)))

(defun company-tern-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (get-text-property 0 'doc candidate))

(defun company-tern-annotation (candidate)
  "Return type annotation for chosen CANDIDATE."
  (get-text-property 0 'type candidate))

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (company-tern-prefix))
    (candidates (cons :async
                      (lambda (callback)
                        (company-tern-candidates-query arg callback))))
    (annotation (company-tern-annotation arg))
    (meta (company-tern-meta arg))))

(provide 'company-tern)

;;; company-tern.el ends here
