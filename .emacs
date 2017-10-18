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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (diminish spaceline evil-magit doom-themes magit evil-leader evil-escape org-bullets evil helm))))

;;------------------------------------------------------------------------
;; DOOM THEMES
;;------------------------------------------------------------------------
(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t 
      doom-themes-enable-italic t)
;; Load the theme
(load-theme 'doom-one t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;;------------------------------------------------------------------------
;; SPACELINE
;;------------------------------------------------------------------------
(require 'spaceline-config)
(spaceline-spacemacs-theme)
;; Separator style
(setq powerline-default-separator 'slant
      powerline-height 20)
(spaceline-compile)
;;(spaceline-toggle-minor-modes-off)
;;(spaceline-toggle-hud-off)
;; Change state, change color
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(set-face-attribute 'spaceline-evil-emacs nil :background "#C678DD")
(set-face-attribute 'spaceline-evil-insert nil :background "#98BE65")
(set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
(set-face-attribute 'spaceline-evil-normal nil :background "#BBC2CF")
(set-face-attribute 'spaceline-evil-replace nil :background "#ECBE7B")
(set-face-attribute 'spaceline-evil-visual nil :background "#DA8548")
(powerline-reset)
;; Diminish power
(require 'diminish)
(when (require 'diminish nil 'noerror)
  (eval-after-load "evil-escape"
    '(diminish 'evil-escape-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode "A"))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode "U")))

;;------------------------------------------------------------------------
;; ORG-MODE
;;------------------------------------------------------------------------
;; Just italic, thanks!
(setq org-hide-emphasis-markers t)
;; Better header bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Closing timestamp
(setq org-log-done 'time)
;; Deactivated for inline images
(setq org-image-actual-width 666)
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
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.3))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))))

;;------------------------------------------------------------------------
;; EVIL-MODE
;;------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
;; Evil with black magic
(require 'evil-magit)
;; Escape with jk
(evil-escape-mode 1)
(setq-default
 evil-escape-key-sequence "jk"
 evil-escape-unordered-key-sequence "true"
 )
;; Leader to lead bindings
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "<SPC>" 'helm-M-x
 "ff" 'helm-find-files
 "fr" 'helm-recentf
 "fs" 'save-buffer
 "bb" 'helm-mini
 "bd" 'kill-this-buffer
 "cl" 'comment-line
 "gs" 'magit-status
 "gl" 'magit-log-all
 "gp" 'magit-push
 "hdf" 'describe-function
 "hdk" 'describe-key
 "ij" 'insert-line-below
 "ik" 'insert-line-above
 "pi" 'package-install
 "pr" 'package-refresh-contents
 "pl" 'package-list-packages
 "qq" 'save-buffers-kill-terminal
 "ti" 'org-toggle-inline-images
 "tl" 'visual-line-mode
 "tm" 'toggle-frame-maximized
 "tb" 'menu-bar-mode
 "u" 'undo-tree-visualize
 "w-" 'split-window-below
 "w/" 'split-window-right
 "wd" 'delete-window
 "wD" 'delete-other-windows
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right
 "y" 'helm-show-kill-ring)
;; evil special key
(define-key evil-normal-state-map (kbd "SPC TAB") 'evil-switch-to-windows-last-buffer)

;;------------------------------------------------------------------------
;; HELM-MODE
;;------------------------------------------------------------------------
(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;; Helm config key
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
;; Helm mini fuzzy matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;;------------------------------------------------------------------------
;; SAVE PLACE
;;------------------------------------------------------------------------
(save-place-mode 1)

;;------------------------------------------------------------------------
;; BACKUP FILES
;;------------------------------------------------------------------------
(defvar backup-dir (expand-file-name "~/.ditch/backup/"))
(defvar autosave-dir (expand-file-name "~/.ditch/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

;;------------------------------------------------------------------------
;; WINDOW ENHANCEMENT
;;------------------------------------------------------------------------
;; Hide (m)anything
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
;; Window geometry
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 75))
;; Scratch message
(setq initial-scratch-message "
;;------------------------------------------------------------------------
;; SANREMEMBER @ EMACSKNIGHT /////////////////////////////////////////////
;;------------------------------------------------------------------------

")

;;------------------------------------------------------------------------
;; INSERT LINE FUNCTION
;;------------------------------------------------------------------------
(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

;;------------------------------------------------------------------------
;; CUSTOM FACES
;;------------------------------------------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro" :height 1.3 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro" :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro" :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro" :height 1.15))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Code Pro")))))
