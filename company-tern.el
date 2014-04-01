;;; company-tern.el --- Tern backend for company-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2013 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/company-tern
;; Version: 0.0.1
;; Package-Requires: ((company "0.6.12") (tern "0.0.1"))

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

(defvar company-tern-complete-on-dot t
  "If not nil, invoke tern completion after dot inserting.")

(defun company-tern-prefix ()
  "Grab prefix at point.
Properly detect strings, comments and attribute access."
  (when (not (company-in-string-or-comment))
    (let ((symbol (company-grab-symbol)))
      (if symbol
          (if (and company-tern-complete-on-dot
                   (save-excursion
                     (forward-char (- (length symbol)))
                     (looking-back "\\." (- (point) 1))))
              (cons symbol t)
            symbol)
        'stop))))

(defun company-tern-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from tern.
Use CALLBACK function to display candidates."
  (setq tern-last-point-pos (point))
  (tern-run-query
   (lambda (data)
     (let* ((start (+ 1 (cdr (assq 'start data))))
            (end (+ 1 (cdr (assq 'end data))))
            (text (buffer-substring-no-properties start end))
            (cs (loop for elt across (cdr (assq 'completions data))
                      collect elt)))
       (setq tern-last-completions (list text start end cs))
       (funcall callback cs)))
   "completions"
   (point)))

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (and tern-mode (company-tern-prefix)))
    (candidates (cons :async
                      (lambda (c)
                        (company-tern-candidates-query arg c))))))

(provide 'company-tern)

;;; company-tern.el ends here
