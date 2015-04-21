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
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-w")
                '(lambda(b e) (interactive "r")
                   (if rm-mark-active
                       (rm-kill-region b e) (kill-region b e))))
(global-set-key (kbd "M-w")
                '(lambda(b e) (interactive "r")
                   (if rm-mark-active
                       (rm-kill-ring-save b e) (kill-ring-save b e))))
(global-set-key (kbd "C-x C-x")
                '(lambda(&optional p) (interactive "p")
                   (if rm-mark-active
                       (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))

;; Line Join
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(provide 'keybindings)
;;; keybindings.el ends here
