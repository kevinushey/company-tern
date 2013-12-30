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

(defvar company-tern-delay 0.5
  "Delay waiting for results from tern.")

;;;###autoload
(defun company-tern (command &optional arg)
  "Tern backend for company-mode.

See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-tern))
    (prefix (and tern-mode (company-grab-symbol)))
    (candidates (let (candidates-list)
                  (tern-run-query (lambda (data)
                                    (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
                                          (start (+ 1 (cdr (assq 'start data))))
                                          (end (+ 1 (cdr (assq 'end data)))))
                                      (setq tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
                                      (setq candidates-list cs)))
                                  "completions"
                                  (point))
                  (sleep-for company-tern-delay)
                  candidates-list))))

(provide 'company-tern)

;;; company-tern.el ends here
