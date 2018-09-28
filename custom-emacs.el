;;; custom-emacs.el --- Custom emacs init file for minimal C/C++ setup
;;; Commentary:
;;; clone to ~.emacs.d/custom-emacs and add the following to ~/.emacs/init.el:
;;;     (add-to-list 'load-path "~/.emacs.d/custom-emacs")
;;;     (require 'custom-emacs)
;;; last updated: 28/9/18

;;; Code:
(require 'package)
(setq package-archives
         '(("melpa" . "https://melpa.org/packages/")))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Appearance/behaviour customisations
(load-theme 'leuven)
(add-to-list 'default-frame-alist
             '(font . "Inconsolata-12")) ;;fonts.google.com/specimen/Inconsolata
(when window-system
  (set-frame-size (selected-frame) 89 33))
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-message t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-linum-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Run make in the current or parent directory
(global-set-key (kbd "<f3>")
                (lambda ()
                  (interactive)
                  (if (file-exists-p "Makefile")
                      (compile "make")
                    (let ((default-directory (expand-file-name "..")))
                      (if (file-exists-p "Makefile")
                          (compile "make")
                        (message "No Makefile found"))))))
;;TODO-default make command if none exists

;; Global and C/C++ packages
(use-package flycheck
  :init
  (global-flycheck-mode))
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends)) ;; use company-clang backend
(use-package ggtags
  :init
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode))
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; C/C++ specific configuration
(defun zw/c-modes-common ()
  "Common settings for C modes."
  (c-set-style "stroustrup")
  (abbrev-mode -1)
  (eldoc-mode 0)
  (column-number-mode t))
(add-hook 'c-mode-hook 'zw/c-modes-common)
(add-hook 'c-mode-hook
          (lambda ()
            ;; set '_' as a word delimiter (for M-f/M-b)
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)))
(add-hook 'c++-mode-hook 'zw/c-modes-common)
(add-hook 'c++-mode-hook
          (lambda ()
            ;; set '_' as a word delimiter (for M-f/M-b)
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/7")))
(provide 'custom-emacs)
;;; custom-emacs.el ends here
