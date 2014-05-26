;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Version: 0.0.1
;; Package-Requires: ((company "0.8.0") (tern "0.0.1") (dash "2.6.0") (s "1.9.0"))

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
(require 'dash)
(require 's)
(eval-when-compile (require 'cl))

(defvar company-tern-own-property-marker " ○"
  "String to indicate object own properties.")

(defvar company-tern-meta-as-single-line nil
  "Trim candidate type information to length of frame width.")

(defun company-tern-prefix ()
  "Grab prefix for tern."
  (and tern-mode
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defun company-tern-format-candidate (completion)
  "Grab candidate with properties from COMPLETION."
  (let ((candidate (cdr (assq 'name completion))))
    (put-text-property 0 1 'type (cdr (assq 'type completion)) candidate)
    (put-text-property 0 1 'doc (cdr (assq 'doc completion)) candidate)
    (put-text-property 0 1 'depth (cdr (assq 'depth completion)) candidate)
    candidate))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (tern-run-query
   (lambda (data)
     (funcall callback
              (company-tern-sort-by-depth
               (mapcar #'company-tern-format-candidate
                       (cdr (assq 'completions data))))))
   '((type . "completions")
     (includeKeywords . t)
     (depths . t)
     (types . t)
     (docs .t))
   (point)))

(defun company-tern-sort-by-depth (candidates)
  "Sort CANDIDATES list by completion depth."
  (let (own-properties prototype-properties)
    (mapcar #'(lambda (candidate)
                (if (company-tern-own-property-p candidate)
                    (push candidate own-properties)
                  (push candidate prototype-properties)))
            (reverse candidates))
    (append own-properties prototype-properties)))

(defun company-tern-own-property-p (candidate)
  "Return t if CANDIDATE is object own property."
  (eq 0 (get-text-property 0 'depth candidate)))

(defun company-tern-doc (candidate)
  "Return documentation buffer for CANDIDATE."
  (-when-let (doc (get-text-property 0 'doc candidate))
    (company-doc-buffer doc)))

(defun company-tern-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (-when-let (type (get-text-property 0 'type candidate))
    (if company-tern-meta-as-single-line
        (substring type 0 (min (frame-width) (length type)))
      type)))

(defun company-tern-annotation (candidate)
  "Return type annotation for chosen CANDIDATE."
  (concat
   (company-tern-get-type candidate)
   (if (company-tern-own-property-p candidate)
       company-tern-own-property-marker
     "")))

(defun company-tern-get-type (candidate)
  "Analyze CANDIDATE type."
  (-when-let (type (get-text-property 0 'type candidate))
    (if (company-tern-function-p type)
        (company-tern-function-type type)
      (company-tern-variable-type type))))

(defun company-tern-function-p (type)
  "Return t if given TYPE is a function."
  (s-starts-with? "fn(" type))

(defun company-tern-function-type (type)
  "(test, context)")

(defun company-tern-variable-type (type)
  (if company-tooltip-align-annotations
      type
    (concat " -> " type)))

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (company-tern-prefix))
    (annotation (company-tern-annotation arg))
    (meta (company-tern-meta arg))
    (doc-buffer (company-tern-doc arg))
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (company-tern-candidates-query arg callback))))))

(provide 'company-tern)

;;; company-tern.el ends here
