;;; custom-init.el -- customized Emacs init options

;;; Commentary:
;;; These options are ones that don't somehow fit under the guise of the other
;;; configuration files.

;;; Code:

;; Show line numbers in the left margin

(global-linum-mode 1)


;; Show a margin at 80 characters
(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-color "black")

(provide 'custom-init)
;;; custom-init.el ends here
