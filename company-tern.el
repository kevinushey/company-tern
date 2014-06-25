;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0") (tern "0.0.1") (dash "2.6.0") (s "1.9.0") (cl-lib "0.5.0"))

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

;; Add `company-tern' to allowed `company-mode' backends list
;;
;;     (add-to-list 'company-backends 'company-tern)
;;
;; If you don't like circles after object's own properties consider less
;; annoying marker for that purpose.
;;
;;     (setq company-tern-own-property-marker "")
;;
;; You can trim too long function signatures to the frame width.
;;
;;     (setq company-tern-meta-as-single-line t)
;;
;; If you doesn't like inline argument annotations appear with
;; corresponding identifiers, then you can to set up the company align
;; option.
;;
;;     (setq company-tooltip-align-annotations t)

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'tern)
(require 'dash)
(require 's)

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

(defun company-tern-format-candidates (data)
  "Grab candidates with properties from tern DATA."
  (let ((completions (cdr (assq 'completions data)))
        (property-p (assq 'isProperty data)))
    (mapcar
     (lambda (completion)
       (let ((candidate (cdr (assq 'name completion))))
         (dolist (prop (push property-p completion))
           (put-text-property 0 1 (car prop) (cdr prop) candidate))
         candidate))
     completions)))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (tern-run-query
   (lambda (data)
     (funcall callback
              (company-tern-sort-by-depth
               (company-tern-format-candidates data))))
   '((type . "completions")
     (includeKeywords . t)
     (depths . t)
     (types . t)
     (docs . t))
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
  (and (get-text-property 0 'isProperty candidate)
       (not (get-text-property 0 'isKeyword candidate))
       (eq 0 (get-text-property 0 'depth candidate))))

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
  (if (company-tern-keyword-p candidate)
      (company-tern-format-keyword)
    (-when-let (type (get-text-property 0 'type candidate))
      (if (company-tern-function-p type)
          (company-tern-function-type type)
        (company-tern-variable-type type)))))

(defun company-tern-keyword-p (candidate)
  "True if CANDIDATE is a keyword."
  (get-text-property 0 'isKeyword candidate))

(defun company-tern-format-keyword ()
  "Format keyword according to `company-tooltip-align-annotations'."
  (if company-tooltip-align-annotations
      ":keyword"
    " -> :keyword"))

(defun company-tern-function-p (type)
  "Return t if given TYPE is a function."
  (s-starts-with? "fn(" type))

(defun company-tern-function-type (type)
  "Prepare function TYPE for company annotation."
  (let* ((data (list (cons 'type type)))
         (types (tern-parse-function-type data))
         (args (mapconcat (lambda (arg) (car arg)) (cadr types) ", "))
         (annot (if company-tooltip-align-annotations "fn(%s)" "(%s)")))
    (format annot args)))

(defun company-tern-variable-type (type)
  "Prepare variable TYPE for company annotation."
  (if company-tooltip-align-annotations
      type
    (concat " -> " type)))

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
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
