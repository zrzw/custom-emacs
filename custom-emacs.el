;;; custom-emacs.el --- Custom emacs init file
;;; Commentary:
;;; last updated: 16/8/18

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
(set-frame-font "Ubuntu Mono 12" nil t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode t)
(setq inhibit-startup-message t)
(when window-system
  (set-frame-size (selected-frame) 85 45))
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))
;; Run make in the current or parent directory
(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (if (file-exists-p "Makefile")
                      (compile "make")
                    (let ((default-directory (expand-file-name "..")))
                      (compile "make")))))

;; Global and C/C++ packages
(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))

(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(use-package ggtags
  :init
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; C/C++ specific configuration
(c-add-style "stroustrup-4-tabs"
             '("stroustrup"
               (indent-tabs-mode . t)
               (c-basic-offset . 4)))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "stroustrup-4-tabs")
            (modify-syntax-entry ?_ "w" c-mode-syntax-table)
            (define-key c-mode-map  [(control tab)] 'company-complete)))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup-4-tabs")
            (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/7")
            (define-key c++-mode-map  [(control tab)] 'company-complete)))

(provide 'custom-emacs)
;;; custom-emacs.el ends here
