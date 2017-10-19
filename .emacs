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

(package-initialize)
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
 '(custom-safe-themes
   (quote
    ("a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "9f569b5e066dd6ca90b3578ff46659bc09a8764e81adf6265626d7dc0fac2a64" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (all-the-icons-dired neotree airline-themes powerline avy diminish evil-magit doom-themes magit evil-leader evil-escape org-bullets evil helm))))

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
;; POWERLINE
;;------------------------------------------------------------------------
(require 'powerline)
(powerline-default-theme)
;; Powerline separator and height
(setq powerline-default-separator 'slant
       powerline-height 20)
;; AIRLINE-THEMES---------------------------------------------------------
(require 'airline-themes)
(load-theme 'airline-doom-one t)
;; nil on minibuffer
  (custom-theme-set-faces
   'airline-doom-one
   `(minibuffer-prompt ((t (:foreground nil :background nil :inherit 'default))))
  )
;; nil to display full dir
(setq airline-display-directory nil)
;; DIMINISH---------------------------------------------------------------
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
(setq org-hide-emphasis-markers t ;; Just italic
      org-ellipsis "  " ;; More cool ellipsis
      org-log-done 'time ;; Closing timestap
      org-image-actual-width 666 ;; Widt size for inline images
      )
;; Real bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; ORG-BULLETS------------------------------------------------------------
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;------------------------------------------------------------------------
;; EVIL-MODE
;;------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
;; Evil respect org tab in terminal
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 
;; evil special key
(define-key evil-normal-state-map (kbd "SPC TAB") 'evil-switch-to-windows-last-buffer)
;; EVIL-MAGIT-------------------------------------------------------------
(require 'evil-magit)
;; EVIL-ESCAPE------------------------------------------------------------
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk"
	      evil-escape-unordered-key-sequence "true"
 )
;; EVIL-LEADER------------------------------------------------------------
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "<SPC>" 'helm-M-x
 "ff" 'helm-find-files
 "fn" 'neotree
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
 "jc" 'evil-avy-goto-char
 "jh" 'avy-org-goto-heading-timer
 "jl" 'evil-avy-goto-line
 "jt" 'evil-avy-goto-char-timer
 "jw" 'evil-avy-goto-word-1
 "pi" 'package-install
 "pr" 'package-refresh-contents
 "pl" 'package-list-packages
 "qq" 'save-buffers-kill-terminal
 "ti" 'org-toggle-inline-images
 "tl" 'visual-line-mode
 "tm" 'toggle-frame-maximized
 "tn" 'neotree-toggle
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
;; NEOTREE
;;------------------------------------------------------------------------
;; Evil use Neotree
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
;; Neotree theme
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;------------------------------------------------------------------------
;; DIRED
;;------------------------------------------------------------------------
;; Hide details
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))
;; ALL-THE-ICONS-DIRED-MODE-----------------------------------------------
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;------------------------------------------------------------------------
;; SAVE-PLACE
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
 '(default ((t (:family "Source Code Pro")))))
