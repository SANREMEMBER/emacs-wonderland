;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------------------- GENERAL CONFIGURATION ----------------------  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (htmlize google-translate deft smex all-the-icons-gnus counsel ivy all-the-icons-dired neotree airline-themes powerline avy diminish evil-magit doom-themes magit evil-leader evil-escape org-bullets evil))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ---------------------- DISPLAY CONFIGURATION ----------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------
;; CUSTOM FACES
;;------------------------------------------------------------------------
(set-frame-font "Source Code Pro 11")

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
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode))
  (eval-after-load "evil-escape"
    '(diminish 'evil-escape-mode))
  (eval-after-load "ivy"
    '(diminish 'ivy-mode))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
   (eval-after-load "simple"
    '(diminish 'visual-line-mode "W")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------- MODES CONFIGURATION ------------------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; ORG-AGENDA-------------------------------------------------------------
(setq org-agenda-files (list "~/Documents/Projects/Kontrol Versi ODF/TODO.org"))
;; (add-to-list 'org-agenda-files "~/blahblahblah.org" 'append)


;; ORG-CAPTURE------------------------------------------------------------
(setq org-capture-templates
      '(
	("j" "Capture journal"
	 entry (file+datetree "/home/sanremember/Nextcloud/Org/Jurnal/1439.org")
	 "* %?"
	 :empty-lines 1)
	("u" "Capture quote"
	 entry (file+headline "/home/sanremember/Nextcloud/Org/Serbaneka/Kutipan.org" "Daftar Kutipan Pilihan")
	 "** %^{Description}\n   :LOGBOOK:\n   %U\n   :END:\n   #+BEGIN_QUOTE\n   %?\n   #+END_QUOTE"
	 :empty-lines 1)
	))

;; ORG-PUBLISH-------------------------------------------------------------
(require 'htmlize)
(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("private"
	 :base-directory "~/Nextcloud/Org/"
	 :base-extension "org"
	 :publishing-directory "~/Nextcloud/WWW/"
	 :recursive t
	 :publishing-function org-html-publish-to-html

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-head "
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link href=\"file:///home/sanremember/Nextcloud/Org/Item/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link href=\"file:///home/sanremember/Nextcloud/Org/Item/extra.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"shortcut icon\" href=\"file:///home/sanremember/Nextcloud/Org/Item/sanremember-circle.png\">
<script src=\"file:///home/sanremember/Nextcloud/Org/Item/script.js\"></script> 
"

	 :with-toc nil
	 :section-numbers nil
	 :html-postamble nil
	 :html-preamble nil
	 :headline-levels 4

	 :author "Hasannudin Amin"
	 :email "sanremember@protonmail.com"

	 :auto-sitemap t
	 :sitemap-filename "Indeks.org"
	 :sitemap-title "Indeks - SANREMEMBER"
	 :sitemap-sort-files anti-chronologically)

	("pages"
	 :base-directory "~/Documents/Pages/org"
	 :base-extension "org"
	 :publishing-directory "~/Documents/Pages"
	 :recursive t
	 :publishing-function org-html-publish-to-html

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-head "
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<link href=\"asset/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link href=\"asset/extra.css\" rel=\"stylesheet\" type=\"text/css\" />
<link href=\"asset/font.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"shortcut icon\" href=\"asset/sanremember-circle.png\">
<script src=\"asset/script.js\"></script> 
"

	 :with-toc nil
	 :section-numbers nil
	 :html-postamble nil
	 :html-preamble nil
	 :headline-levels 4

	 :author "Hasannudin Amin"
	 :email "sanremember@protonmail.com"

	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Indeks - SANREMEMBER"
	 :sitemap-sort-files anti-chronologically)

	("media"
	 :base-directory "~/Nextcloud/Org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|svg\\|gif"
	 :publishing-directory "~/Nextcloud/WWW/"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("craft" :components ("htmlfly" "media"))))

;;------------------------------------------------------------------------
;; EVIL-MODE
;;------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
;; Evil respect org tab in terminal
(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
	    (define-key evil-normal-state-map (kbd "RET") 'org-open-at-point))) 
;; Default state for some modes
(setq evil-insert-state-modes '(deft-mode))
;; Evil no abbrev again
;; (setq evil-want-abbrev-expand-on-insert-exit nil)
;; Evil special key
(define-key evil-normal-state-map (kbd "SPC TAB") 'evil-switch-to-windows-last-buffer)
;; EVIL-MAGIT-------------------------------------------------------------
(require 'evil-magit)
;; EVIL-ESCAPE------------------------------------------------------------
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk"
	      evil-escape-unordered-key-sequence "true")
;; EVIL-LEADER------------------------------------------------------------
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "<SPC>" 'counsel-M-x
 "ad" 'deft                              ;; A
 "am" 'gnus
 "ata" 'google-translate-at-point
 "atq" 'google-translate-query-translate
 "atQ" 'google-translate-query-translate-reverse
 "bb" 'ivy-switch-buffer		;; B
 "bd" 'kill-this-buffer
 "cb" 'comment-box			;; C
 "cl" 'comment-line
 "ff" 'counsel-find-file		;; F
 "fl" 'counsel-find-library
 "fn" 'neotree
 "fr" 'counsel-recentf
 "fs" 'save-buffer
 "gs" 'magit-status			;; G
 "gl" 'magit-log-all
 "gp" 'magit-push
 "hdb" 'counsel-descbinds		;; H
 "hdf" 'counsel-describe-function
 "hdk" 'describe-key
 "hdv" 'counsel-describe-variable
 "ij" 'insert-line-below		;; I
 "ik" 'insert-line-above
 "jc" 'evil-avy-goto-char		;; J
 "jl" 'evil-avy-goto-line
 "jt" 'evil-avy-goto-char-timer
 "jw" 'evil-avy-goto-word-1
 "oa" 'org-agenda			;; O
 "ol" 'org-agenda-list
 "oc" 'org-capture
 "op" 'org-publish
 "pa" 'package-autoremove		;; P
 "pd" 'package-delete
 "pi" 'package-install
 "pr" 'package-refresh-contents
 "pl" 'package-list-packages
 "qq" 'save-buffers-kill-terminal	;; Q
 "sa" 'counsel-ag			;; S
 "sl" 'counsel-locate
 "so" 'counsel-outline
 "ss" 'swiper
 "ti" 'org-toggle-inline-images		;; T
 "tl" 'visual-line-mode
 "tm" 'toggle-frame-maximized
 "tn" 'neotree-toggle
 "tb" 'menu-bar-mode
 "u" 'undo-tree-visualize		;; U
 "w-" 'split-window-below		;; W
 "w/" 'split-window-right
 "wd" 'delete-window
 "wD" 'delete-other-windows
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right
 "y" 'counsel-yank-pop)			;; Y
;; Special key just for some modes
(evil-leader/set-key-for-mode 'org-mode
  "jh" 'counsel-org-goto
  "oed" 'org-export-dispatch
  "oeh" 'org-html-export-to-html
  "oes" 'org-edit-special
  "oec" 'org-edit-src-exit
  "oeo" 'org-odt-export-to-odt
  "oem" 'org-md-export-to-markdown
  "oib" 'org-tree-to-indirect-buffer
  "oil" 'org-insert-link
  "ost" 'org-set-tags
)

;;------------------------------------------------------------------------
;; IVY
;;------------------------------------------------------------------------
(ivy-mode 1)
;; add recentf and bookmark to mini
(setq ivy-use-virtual-buffers t ;; no regexp by default
      ivy-initial-inputs-alist nil ;; no regexp by default
      ivy-re-builders-alist ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
 
;;------------------------------------------------------------------------
;; SMEX
;;------------------------------------------------------------------------
(require 'smex)
(smex-initialize)

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
;; GNUS
;;------------------------------------------------------------------------
;; Nil method
(setq gnus-select-method '(nnnil))
;; Get email, and store in nnml
(setq gnus-secondary-select-methods
      '(
	(nnimap "gmail"
		(nnimap-address
		 "imap.gmail.com")
		(nnimap-server-port "993")
		(nnimap-stream ssl))
	))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
;; Send email via Gmail:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")
;; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Surat Terkirim")
;; store email in ~/gmail directory
(setq nnml-directory "~/Documents/Mail")
(setq message-directory "~/Documents/Mail")
(setq gnus-directory "~/Documents/Mail")
;; Use tree view
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; Sort by recent date
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))
;; ALL-THE-ICONS-GNUS-----------------------------------------------------
(require 'all-the-icons-gnus)
(all-the-icons-gnus-setup)

;;------------------------------------------------------------------------
;; DEFT
;;------------------------------------------------------------------------
(require 'deft)
(setq deft-directory "~/Nextcloud/Org/Serbaneka"
      deft-extensions '("org" "md" "txt")
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t)

;;------------------------------------------------------------------------
;; GOOGLE-TRANSLATE
;;------------------------------------------------------------------------
(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "id")

;;------------------------------------------------------------------------
;; ISPELL
;;------------------------------------------------------------------------
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=id"))))

;;------------------------------------------------------------------------
;; DIRED
;;------------------------------------------------------------------------
;; Hide details
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))
;; ALL-THE-ICONS-DIRED-MODE-----------------------------------------------
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------ PERSONAL FUNCTION ------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; ISPELL SKIP FOR ORG FUNCTION
;;------------------------------------------------------------------------
(defun ispell-skip-for-org ()
  "Configure ispell-skip-region-alist for org-mode."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'ispell-skip-for-org)

;;------------------------------------------------------------------------
;; AUTO MOVE NEW WINDOW ADVISING FUNCTION
;;------------------------------------------------------------------------
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------- MISCELLANEOUS  --------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; No times for yes
(fset 'yes-or-no-p 'y-or-n-p)

;; Load secret weapon
(load-file "~/.secretweapon.el")

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

