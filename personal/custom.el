;;; custom-init.el -- customized Emacs init options

;;; Commentary:
;;; These options are ones that don't somehow fit under the guise of the other
;;; configuration files.

(prefer-coding-system 'utf-8)

;; Additional repos
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

; install aditional packages
(prelude-require-packages '(powershell csharp-mode jedi fill-column-indicator mark-multiple phi-rectangle flymake smex imenu org rust-mode cargo))


;; Show line numbers in left margin
(global-linum-mode 1)


;; disable whitespace indicator
(setq prelude-whitespace nil)

;; Set global font
(when (eq system-type 'gnu/linux)
  (set-frame-font "Ubuntu Mono-12" nil)) 
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "Consolas" :height 100))
(when (eq system-type 'darwin)
  (set-frame-font "Monaco-11" nil)
  ;; set keys for Apple keyboard, for emacs in OS X
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper))  ; make Fn key do Hyper


;; Reopen read-only files in tramp-mode
(defun th-rename-tramp-buffer ()
  ;; Override for find-file to open with tramp
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(provide 'custom-init)



;; Backup files
(setq user-emacs-directory "~/.emacs.d")

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))



;; Visual Basic
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
                                visual-basic-mode)) auto-mode-alist))


; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)


;;ido mode
(require 'ido)
(ido-mode t)


;; smex
(require 'smex)
(smex-initialize)


;; column number mode
(column-number-mode)
(setq default-tab-width 4)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "\M- ") 'hippie-expand)


;; jedi python completion
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:environment-root "jedi")  ; or any other name you like
(setq jedi:environment-virtualenv
      (list "pyvenv" "--system-site-packages"))


;; Auto-wrap at 80 char in Org Mode files
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          '(lambda() (set-fill-column 80)))

(add-hook 'gfm-mode-hook
          '(lambda() (set-fill-column 80)))


;; Enable irony mode for C/C++
(add-hook 'c-mode-hook 'my-c-mode-hook)
(defun my-c-mode-hook ()
  (irony-mode 1)
  (smartparens-mode -1))

;; Insert the date
(require 'calendar)
  (defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))

;; Just for fun
(global-set-key (kbd "C-c C-c z") 'zone)

<<<<<<< HEAD
;; Disable smartparens and electric pair
(eval-after-load 'smartparens  ;; attempt to crippler smartparens if it can't be disabled
    '(progn
       (sp-pair "(" nil :actions :rem)
       (sp-pair "[" nil :actions :rem)
       (sp-pair "'" nil :actions :rem)
       (sp-pair "\"" nil :actions :rem)))
(smartparens-global-mode -1)
(turn-off-smartparens-mode)
(electric-pair-mode)
=======
;; Enable electric-pair
(electric-pair-mode)

(server-start)

>>>>>>> 24bc721f4b3e7b5080350394429b43a374e4612e

(provide 'custom)
;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
<<<<<<< HEAD
    (jedi which-key smart-mode-line imenu-anywhere editorconfig crux beacon zop-to-char zenburn-theme yasnippet web-beautify volatile-highlights vkill undo-tree tidy smex smartrep smartparens slime roguel-ike rect-mark rainbow-mode rainbow-delimiters python-environment powershell phi-rectangle ov operate-on-number move-text minimap markdown-mode mark-multiple magit lua-mode key-chord json-rpc json-mode js2-mode jinja2-mode ido-completing-read+ guru-mode grizzl gotest god-mode go-projectile gitignore-mode gitconfig-mode git-timemachine git-rebase-mode git-commit-mode gist geiser fsharp-mode flx-ido fill-column-indicator expand-region exec-path-from-shell erlang epc elisp-slime-nav easy-kill discover-my-major diminish diff-hl csv-mode csharp-mode company-irony company-go company-anaconda cmake-mode cider chess cargo browse-kill-ring autopair anzu ack-and-a-half ace-window ace-jump-mode ace-jump-buffer ac-racer ac-anaconda))))
=======
    (erlang slime cider clojure-mode which-key super-save imenu-anywhere hl-todo editorconfig crux beacon zop-to-char zenburn-theme volatile-highlights undo-tree smex smartrep smartparens rainbow-mode rainbow-delimiters powershell phi-rectangle ov operate-on-number move-text mark-multiple magit key-chord json-mode js2-mode jedi ido-ubiquitous guru-mode grizzl gotest god-mode go-projectile gitignore-mode gitconfig-mode git-timemachine gist geiser flycheck flx-ido fill-column-indicator expand-region elisp-slime-nav easy-kill discover-my-major diminish diff-hl csharp-mode company-go company-anaconda cargo browse-kill-ring anzu ace-window ace-jump-mode ace-jump-buffer))))
>>>>>>> 24bc721f4b3e7b5080350394429b43a374e4612e
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
