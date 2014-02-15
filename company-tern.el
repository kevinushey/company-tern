;;; company-tern.el --- Tern backend for company-mode

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

(defvar company-tern-candidates-cache nil
  "Local cache stored last completions from company candidates.")

(defvar company-tern-modified-tick nil
  "Last buffer chars modified tick used for company-tern completions.")

(defvar company-tern-last-prefix nil
  "Last prefix used for company-tern completions.")

(defvar company-tern-last-buffer nil
  "Last buffer in witch company-tern was called.")

(defun company-tern-candidates-p (prefix)
  "Check if tern cache was properly populated with PREFIX."
  (and (eq tern-last-point-pos (point))
       (eq company-tern-modified-tick (buffer-chars-modified-tick))
       (eq company-tern-last-buffer (current-buffer))
       (string= company-tern-last-prefix prefix)))

(defun company-tern-candidates-query (prefix)
  "Retrieve PREFIX completion candidates from tern."
  (setq tern-last-point-pos (point))
  (setq company-tern-modified-tick (buffer-chars-modified-tick))
  (setq company-tern-last-prefix prefix)
  (setq company-tern-last-buffer (current-buffer))
  ;; Do tern call.
  (tern-run-query
   (lambda (data)
     (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
           (start (+ 1 (cdr (assq 'start data))))
           (end (+ 1 (cdr (assq 'end data)))))
       (setq tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
       (setq company-tern-candidates-cache cs)
       ;; Restart company completion.
       (unless company-candidates
         (company-pre-command)
         (company-auto-begin)
         (company-post-command))))
   "completions"
   (point))
  ;; Skip company-tern at that time.
  nil)

(defun company-tern-candidates (prefix)
  "Start asynchronous tern completion with PREFIX."
  (if (company-tern-candidates-p prefix)
      company-tern-candidates-cache
    (company-tern-candidates-query prefix)))

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (and tern-mode (company-tern-prefix)))
    (candidates (company-tern-candidates arg))))

(provide 'company-tern)

;;; company-tern.el ends here
