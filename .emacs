(package-initialize)

;;------------------------------------------------------------------------
;; PERSONAL INFORMATION
;;------------------------------------------------------------------------
(setq user-full-name "Hasannudin Amin"
      user-mail-address "sanremember@protonmail.com")

;;------------------------------------------------------------------------
;; PACKAGE SOURCES
;;------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;;------------------------------------------------------------------------
;; CUSTOM VARIABLES
;;------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (evil-escape org-bullets evil helm))))

;;------------------------------------------------------------------------
;; ORG-MODE
;;------------------------------------------------------------------------
;; Just italic, thanks!
(setq org-hide-emphasis-markers t)
;; Better header bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Real bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; Change font faces
(let* ((variable-tuple (cond ((x-list-fonts "Source Code Pro") '(:font "Source Code Pro"))
                             ((x-list-fonts "Liberation Mono") '(:font "Liberation Mono"))
                             ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;;------------------------------------------------------------------------
;; EVIL-MODE
;;------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
;; Escape with jk
(evil-escape-mode 1)
(setq-default
 evil-escape-key-sequence "jk"
 evil-escape-unordered-key-sequence "true"
 )

;;------------------------------------------------------------------------
;; HELM-MODE
;;------------------------------------------------------------------------
(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-,") 'helm-select-action)
;; Helm config key
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)


;;------------------------------------------------------------------------
;; BACKUP FILES
;;------------------------------------------------------------------------
(defvar backup-dir (expand-file-name "~/ditch/backup/"))
(defvar autosave-dir (expand-file-name "~/ditch/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

;;------------------------------------------------------------------------
;; MENU ELEMENT
;;------------------------------------------------------------------------
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
