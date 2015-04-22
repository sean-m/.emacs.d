;;; keybindings.el --- Custom keybindings go here
;;; Commentary:

;;; Code:

;; Comment region

(global-set-key (kbd "C-c C-e") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)


;; Make rectangle selections visible
(require 'mark-multiple)
(require 'phi-rectangle)
(phi-rectangle-mode)
(global-set-key (kbd "C-x r C-SPC") 'phi-rectangle-set-mark-command)

;; Line Join
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(provide 'keybindings)
;;; keybindings.el ends here
