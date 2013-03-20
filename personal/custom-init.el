;;; custom-init.el -- customized Emacs init options

;;; Commentary:
;;; These options are ones that don't somehow fit under the guise of the other
;;; configuration files.

;;; Code:
(load-library "~/.emacs.d/personal/repo.el")

;; Show line numbers in the left margin
(package-initialize)

(defvar personal-packages
             '(fill-column-indicator mark-multiple rect-mark flymake))

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
(setq fci-rule-width 2)
(setq fci-rule-color "black")

;; Set global font
(set-frame-font "Consolas-9" nil)

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
