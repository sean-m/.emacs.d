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

(global-linum-mode 1)


;; Show a margin at 80 characters
(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-color "black")

(provide 'custom-init)
;;; custom-init.el ends here
