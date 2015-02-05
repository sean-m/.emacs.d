;;; custom-init.el -- customized Emacs init options

;;; Commentary:
;;; These options are ones that don't somehow fit under the guise of the other
;;; configuration files.

;;; Code:
(load-library "~/.emacs.d/personal/repo.el")

;; Show line numbers in the left margin
(package-initialize)

(defvar personal-packages
             '(fill-column-indicator mark-multiple rect-mark flymake smex))

;; This block was taken from the a Prelude init script and modified

(defun personal-packages-installed-p ()
  ;; check if the listed packages are installed
  (loop for p in personal-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (personal-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p personal-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Show line numbers in left margin
(global-linum-mode 1)


;; Show a margin at 80 characters
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "grey")
(fci-mode)

;; disable whitespace indicator
(setq prelude-whitespace nil)

;; Set global font
(when (eq system-type 'gnu/linux)
  (set-frame-font "Ubuntu Mono-12" nil)) 
(when (eq system-type 'windows-nt)
    (set-frame-font "Consolas-10" nil))
(when (eq system-type 'darwin)
  (set-frame-font "Monaco-10" nil)
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
;;; custom-init.el ends here


;; Open file as root?
(defun th-rename-tramp-buffer ()
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


;; Backup files
(setq user-emacs-directory "~/.emacs.d")

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))


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
(add-hook 'python-mode-hook 'jedi:ac-setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


;; Disable quote completion
(require 'smartparens)
(setq sp-autoescape-string-quote nil)


;; Auto-wrap at 80 char in Org Mode files
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          '(lambda() (set-fill-column 80)))


;; Insert the date
(require 'calendar)
  (defun insdate-insert-current-date (&optional omit-day-of-week-p)
    "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
    (interactive "P*")
    (insert (calendar-date-string (calendar-current-date) nil
				  omit-day-of-week-p)))
