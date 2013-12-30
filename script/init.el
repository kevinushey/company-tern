(require 'tern)
(add-hook 'js-mode-hook 'tern-mode)

(require 'company)
(global-company-mode)

(add-to-list 'company-backends 'company-tern)
