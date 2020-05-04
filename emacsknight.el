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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

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
 '(ledger-reports
   (quote
    (("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(package-selected-packages
   (quote
    (yapfify company php-mode emojify easy-hugo org-fancy-priorities highlight-indent-guides engine-mode anaconda-mode yaml-mode ace-window ledger-mode web-beautify emmet-mode rainbow-mode writeroom-mode ox-pandoc pandoc-mode ox-twbs fasd markdown-mode which-key htmlize google-translate deft smex all-the-icons-gnus counsel ivy all-the-icons-dired neotree airline-themes powerline avy diminish evil-magit doom-themes magit evil-leader evil-escape org-bullets evil))))




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
;; Default title
(setq-default frame-title-format '("%b - GNU Emacs"))
;; Hide (m)anything
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
;; Window geometry
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 75))
;; Scratch buffer mode
(setq initial-major-mode 'org-mode)
;; Scratch message
(setq initial-scratch-message "
;;             ███████╗███╗   ███╗ █████╗  ██████╗███████╗              ;;
;;             ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝              ;;
;;             █████╗  ██╔████╔██║███████║██║     ███████╗              ;;
;;             ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║              ;;
;;             ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║              ;;
;;             ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝              ;;
                                           
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
;; Cure for icons in emacs
(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

;; DIMINISH---------------------------------------------------------------
(require 'diminish)
(when (require 'diminish nil 'noerror)
  (eval-after-load "all-the-icons-dired"
    '(diminish 'all-the-icons-dired-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))
  (eval-after-load "anaconda-mode"
    '(diminish 'anaconda-mode "A"))
  (eval-after-load "company"
    '(diminish 'company-mode "C"))
  (eval-after-load "evil-escape"
    '(diminish 'evil-escape-mode))
  (eval-after-load "org-fancy-priorities"
    '(diminish 'org-fancy-priorities-mode))
  (eval-after-load "ivy"
    '(diminish 'ivy-mode))
  (eval-after-load "pandoc-mode"
    '(diminish 'pandoc-mode "P"))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))
  (eval-after-load "simple"
    '(diminish 'visual-line-mode "W"))
  (eval-after-load "which-key"
    '(diminish 'which-key-mode)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------- MODES CONFIGURATION ------------------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------
;; ORG-MODE
;;------------------------------------------------------------------------
(setq org-hide-emphasis-markers t ;; Just italic
      org-ellipsis "  " ;; More cool ellipsis
      org-log-done 'time ;; Closing timestamp
      org-image-actual-width 666 ;; Widt size for inline images
      org-src-fontify-natively t ;; Highlight source
      org-src-tab-acts-natively t ;; Native tabs for source
      )
;; Real bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; ORG-BULLETS------------------------------------------------------------
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; ORG-FANCY-PRIORITIES----------------------------------------------------
(require 'org-fancy-priorities)
(add-hook 'org-mode-hook (lambda () (org-fancy-priorities-mode 1)))
(setq org-fancy-priorities-list '("❗" "⮬" "⮮" "☕"))
;; ORG-AGENDA-------------------------------------------------------------
(setq org-agenda-files (list "~/Documents/Projects/Kontrol Versi ODF/TODO.org"))
;; (add-to-list 'org-agenda-files "~/blahblahblah.org" 'append)


;; ORG-CAPTURE------------------------------------------------------------
(setq org-capture-templates
      '(
	("j" "Capture journal"
	 entry (file+headline "/home/sanremember/Documents/Organize/Agenda/Now.org" "Dzulhijjah 1439")
	 "** %^{Date} Dzulhijjah\n*** %^{Headline}\n    :LOGBOOK:\n    %U\n    :END:\n    %?"
	 :empty-lines 1)
	("u" "Capture quote"
	 entry (file+headline "/home/sanremember/Nextcloud/Org/Serbaneka/Kutipan.org" "Daftar Kutipan Pilihan")
	 "** %^{Description}\n   :LOGBOOK:\n   %U\n   :END:\n   #+BEGIN_QUOTE\n   %?\n   #+END_QUOTE"
	 :empty-lines 1)
	))

;; ORG-BABEL---------------------------------------------------------------
;; (org-babel-do-load-languages
;;  'org-bable-load-languages
;;  '((ledger . t)
;;    (ditaa . t)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ledger . t)
     (plantuml . t)
     )))

;; ORG-PUBLISH-------------------------------------------------------------
(require 'htmlize)
(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("pages"
	 :base-directory "~/Documents/Blog/Org"
	 :base-extension "org"
	 :publishing-directory "~/Documents/Blog/Org/publish"
	 :recursive t
	 :publishing-function org-html-publish-to-html

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-head "
<link rel=\"stylesheet\" href=\"https://overpass-30e2.kxcdn.com/overpass.css\">
<link rel=\"stylesheet\" href=\"https://overpass-30e2.kxcdn.com/overpass-mono.css\">
<link href=\"media/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"shortcut icon\" href=\"media/sanremember.png\">
"

	 :with-toc nil
	 :section-numbers nil
	 :html-postamble "
<div class=\"ending\"><p> © <a href= \"/\">Hasannudin Amin</a> - Hanya Hamba Allah Subhanahu wa Ta'ala<p/></div>
<script src=\"media/instantclick.min.js\" data-no-instant></script>
<script data-no-instant>InstantClick.init();</script>
"
	 :html-preamble nil
	 :headline-levels 4

	 :author "Hasannudin Amin"
	 :email "sanremember@protonmail.com"

	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "SANREMEMBER"
	 :sitemap-sort-files anti-chronologically)

	("media"
	 :base-directory "~/Documents/Blog/Org/media"
	 :base-extension "css\\|js\\|png\\|jpg\\|svg\\|gif"
	 :publishing-directory "~/Documents/Blog/Org/publish/media"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("craft" :components ("pages" "media"))))

;; ABBREV-MODE------------------------------------------------------------
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

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
(add-to-list 'evil-insert-state-modes 'deft-mode)
;; Evil no abbrev again
(setq evil-want-abbrev-expand-on-insert-exit nil)
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
 "aa"    'ansi-term                     ;; A
 "ac"	 'calc-dispatch
 "ad"	 'deft
 "ae"    'eshell
 "am"	 'gnus
 "ata"	 'google-translate-at-point
 "atq"	 'google-translate-query-translate
 "atQ"	 'google-translate-query-translate-reverse
 "bb"	 'ivy-switch-buffer		;; B
 "bd"	 'kill-this-buffer
 "cb"	 'comment-box			;; C
 "cl"	 'comment-line
 "ebl"	 'bookmark-bmenu-list           ;; E
 "ebs"   'bookmark-set
 "ed"	 'find-emacs-dir
 "ei"	 'find-emacs-init
 "el"	 'counsel-find-library
 "et"	 'counsel-load-theme
 "epa"	 'package-autoremove
 "epd"	 'package-delete
 "epi"	 'package-install
 "epr"	 'package-refresh-contents
 "epl"	 'package-list-packages
 "eq"	 'save-buffers-kill-terminal
 "ff"	 'counsel-find-file	        ;; F
 "fn"	 'neotree
 "fr"	 'counsel-recentf
 "fs"	 'save-buffer
 "gs"	 'magit-status			;; G
 "gl"	 'magit-log-all
 "gp"	 'magit-push
 "hb"	 'counsel-descbinds		;; H
 "hf"	 'counsel-describe-function
 "hk"	 'describe-key
 "hv"	 'counsel-describe-variable
 "ie"	 'emojify-insert-emoji		;; I
 "ij"	 'insert-line-below
 "ik"	 'insert-line-above
 "jc"	 'evil-avy-goto-char		;; J
 "jl"	 'evil-avy-goto-line
 "jt"	 'evil-avy-goto-char-timer
 "jw"	 'evil-avy-goto-word-1
 "oa"	 'org-agenda			;; O
 "ol"	 'org-agenda-list
 "oc"	 'org-capture
 "op"	 'org-publish
 "osc"   'org-edit-src-exit
 "sa"	 'counsel-ag			;; S
 "sf"    'fasd-find-file
 "sl"	 'counsel-locate
 "so"	 'counsel-outline
 "ss"	 'swiper
 "tln"	 'linum-mode                    ;; T
 "tlw"	 'visual-line-mode
 "tm"	 'toggle-frame-maximized
 "tn"	 'neotree-toggle
 "tb"	 'menu-bar-mode
 "tw"	 'writeroom-mode
 "u"	 'undo-tree-visualize		;; U
 "w-"	 'split-window-below		;; W
 "w/"	 'split-window-right
 "wa"    'ace-window
 "wd"	 'delete-window
 "wD"	 'delete-other-windows
 "wh"	 'evil-window-left
 "wj"	 'evil-window-down
 "wk"	 'evil-window-up
 "wl"	 'evil-window-right
 "ws"    'toggle-window-split
 "y"	 'counsel-yank-pop)		;; Y

;; Special key just for org-mode
(evil-leader/set-key-for-mode 'org-mode
  "jh"	'counsel-org-goto
  "ob"	'org-tree-to-indirect-buffer
  "oed" 'org-export-dispatch
  "oeh" 'org-html-export-to-html
  "oem" 'org-md-export-to-markdown
  "oeo" 'org-odt-export-to-odt
  "oil" 'org-insert-link
  "oit" 'org-time-stamp
  "ose" 'org-edit-special
  "ot"	'org-set-tags
  "ti"	'org-toggle-inline-images)

;; Special key just for python-mode
(evil-leader/set-key-for-mode 'python-mode
  "pd" 'anaconda-mode-show-doc
  "pfa" 'anaconda-mode-find-assignments
  "pfd" 'anaconda-mode-find-definitions
  "pfr" 'anaconda-mode-find-references
  "pic" 'python-skeleton-class
  "pid" 'python-skeleton-def
  "pif" 'python-skeleton-for
  "pii" 'python-skeleton-if
  "pim" 'python-skeleton-import
  "pit" 'python-skeleton-try
  "piw" 'python-skeleton-while
  "pr"	'run-python
  "psb" 'python-shell-send-buffer
  "psf" 'python-shell-send-file
  "psr" 'python-shell-send-region)

;; Special key just for markdown-mode
(evil-leader/set-key-for-mode 'markdown-mode
  "m-"	'markdown-insert-hr
  "mil"	'markdown-insert-link
  "miu" 'markdown-insert-uri
  "mif" 'markdown-insert-footnote
  "mii" 'markdown-insert-image
  "miI" 'markdown-insert-reference-image
  "miw" 'markdown-insert-wiki-link
  "mvb" 'markdown-insert-bold
  "mvi" 'markdown-insert-italic
  "mvc" 'markdown-insert-code
  "mvq" 'markdown-insert-blockquote
  "mvp" 'markdown-insert-pre)

;; Special key just for ledger-mode
(evil-leader/set-key-for-mode 'ledger-mode
  "la"  'ledger-add-transaction
  "lc"  'ledger-mode-clean-buffer
  "lr"  'ledger-report)

;;------------------------------------------------------------------------
;; WHICH-KEY
;;------------------------------------------------------------------------
(which-key-mode 1)
;; Which key as minibuffer
(which-key-setup-minibuffer)
;; Declare prefix for general
(dolist (pf '("SPC " "M-x "))
  (which-key-declare-prefixes
    (concat pf "SPC")	"M-x"
    (concat pf "TAB")	"last buffer"
    (concat pf "a")	"apps"		;; A
    (concat pf "ac")    "calc"
    (concat pf "am")    "mail"
    (concat pf "at")	"translate"
    (concat pf "ata")	"at point"
    (concat pf "atq")	"query"
    (concat pf "atQ")	"query reverse"
    (concat pf "b")	"buffer"	;; B
    (concat pf "bb")	"buffers"
    (concat pf "bd")	"delete"
    (concat pf "c")	"comment"	;; C
    (concat pf "cb")	"box"
    (concat pf "cl")	"line"
    (concat pf "e")	"emacs"		;; E
    (concat pf "eb")	"bookmark"
    (concat pf "ebl")	"list"
    (concat pf "ebs")	"set"
    (concat pf "ed")	"directory"
    (concat pf "ei")	"init file"
    (concat pf "el")	"library"
    (concat pf "ep")	"package"
    (concat pf "epa")	"autoremove"
    (concat pf "epd")	"delete"
    (concat pf "epi")	"install"
    (concat pf "epr")	"refresh"
    (concat pf "epl")	"list"
    (concat pf "eq")	"quit"
    (concat pf "et")	"themes"
    (concat pf "f")	"file"		;; F
    (concat pf "ff")	"find"
    (concat pf "fr")	"recent"
    (concat pf "fs")	"save"
    (concat pf "g")	"git"		;; G
    (concat pf "gs")	"status"
    (concat pf "gl")	"log"
    (concat pf "gp")	"push"
    (concat pf "h")	"help"		;; H
    (concat pf "hb")	"binding"
    (concat pf "hf")	"function"
    (concat pf "hk")	"key"
    (concat pf "hv")	"variable"
    (concat pf "i")	"insert"	;; I
    (concat pf "ie")	"emoji"
    (concat pf "ij")	"below"
    (concat pf "ik")	"above"
    (concat pf "j")	"jump"		;; J
    (concat pf "jc")	"char"
    (concat pf "jl")	"line"
    (concat pf "jt")	"timer"
    (concat pf "jw")	"word"
    (concat pf "o")	"org"		;; O
    (concat pf "oa")	"agenda"
    (concat pf "oc")	"capture"
    (concat pf "ol")	"list agenda"
    (concat pf "os")	"source"
    (concat pf "osc")	"close"
    (concat pf "op")	"publish"
    (concat pf "s")	"search"	;; S
    (concat pf "sa")	"ag"
    (concat pf "sf")    "fasd"
    (concat pf "sl")	"locate"
    (concat pf "so")	"outline"
    (concat pf "ss")	"swiper"
    (concat pf "t")	"toggle"	;; T
    (concat pf "tl")	"line"
    (concat pf "tln")	"number"
    (concat pf "tlw")	"wrap"
    (concat pf "tm")	"maximize"
    (concat pf "tn")	"neotree"
    (concat pf "tb")	"bar"
    (concat pf "tw")	"writeroom"
    (concat pf "u")	"undo"		;; U
    (concat pf "w")	"window"	;; W
    (concat pf "w-")	"horizontal"
    (concat pf "w/")	"vertical"
    (concat pf "wa")	"ace"
    (concat pf "wd")	"delete"
    (concat pf "wD")	"delete others"
    (concat pf "wh")	"left"
    (concat pf "wj")	"down"
    (concat pf "wk")	"up"
    (concat pf "wl")	"right"
    (concat pf "ws")    "switch split"
    (concat pf "y")	"yank"		;; Y
    ))

;; Declare prefix for org
(dolist (pf '("SPC " "M-x "))
  (which-key-declare-prefixes-for-mode 'org-mode
    (concat pf "jh")	"heading"
    (concat pf "ob")	"buffer"
    (concat pf "oe")	"export"
    (concat pf "oed")	"dispatch"
    (concat pf "oeh")	"html"
    (concat pf "oeo")	"odt"
    (concat pf "oem")	"markdown"
    (concat pf "oi")	"insert"
    (concat pf "oil")	"link"
    (concat pf "oit")	"time"
    (concat pf "ose")	"edit"
    (concat pf "ot")	"tags"
    (concat pf "ti")	"images"
    ))

;; Declare prefix for python
(dolist (pf '("SPC " "M-x "))
  (which-key-declare-prefixes-for-mode 'python-mode
    (concat pf "p")	"python"
    (concat pf "pd")	"documentation"
    (concat pf "pf")	"find"
    (concat pf "pfa")	"assignment"
    (concat pf "pfd")	"definition"
    (concat pf "pfr")	"reference"
    (concat pf "pi")	"insert"
    (concat pf "pic")	"class"
    (concat pf "pid")	"def"
    (concat pf "pif")	"for"
    (concat pf "pii")	"if"
    (concat pf "pim")	"import"
    (concat pf "pit")	"try"
    (concat pf "piw")	"while"
    (concat pf "pr")	"run"
    (concat pf "ps")	"send"
    (concat pf "psb")	"buffer"
    (concat pf "psf")	"file"
    (concat pf "psr")	"region"
    ))

;; Declare prefix for markdown
(dolist (pf '("SPC " "M-x "))
  (which-key-declare-prefixes-for-mode 'markdown-mode
    (concat pf "m")	"markdown"
    (concat pf "m-")	"hr line"
    (concat pf "mi")	"insert"
    (concat pf "mil")	"link"
    (concat pf "miu")	"uri"
    (concat pf "mif")	"footnote"
    (concat pf "mii")	"image"
    (concat pf "miI")	"reference image"
    (concat pf "miw")	"wiki link"
    (concat pf "mv")	"visual"
    (concat pf "mvb")	"bold"
    (concat pf "mvi")	"italic"
    (concat pf "mvc")	"code"
    (concat pf "mvq")	"quote"
    (concat pf "mvp")	"pre"
    ))

;; Declare prefix for ledger
(dolist (pf '("SPC " "M-x "))
  (which-key-declare-prefixes-for-mode 'ledger-mode
    (concat pf "l")     "ledger"
    (concat pf "la")    "add transaction"
    (concat pf "lc")    "clean"
    (concat pf "lr")    "report"
    ))


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
;; WRITEROOM
;;------------------------------------------------------------------------
;; Key for change size margin
(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-=") #'writeroom-adjust-width))

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
;; EMMET
;;------------------------------------------------------------------------
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
;; Move cursor to entire quotes
(setq emmet-move-cursor-between-quotes t)

;;------------------------------------------------------------------------
;; LEDGER
;;------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
;; States for ledger report
(add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
;; Align ammount
(setq ledger-post-auto-adjust-amounts t)

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
;; Set visibile header
(setq gnus-visible-headers
"^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")
;; Sort visible header
 (setq gnus-sorted-header-list
            '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
"^Subject:" "^Date:" "^Gnus"))
;; ALL-THE-ICONS-GNUS-----------------------------------------------------
(require 'all-the-icons-gnus)
(all-the-icons-gnus-setup)
;; HYDRA------------------------------------------------------------------
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups")
       ("l" gnus-group-list-all-groups "LOCAL groups")
       ("c" gnus-topic-catchup-articles "Read all")
       ("G" gnus-group-make-nnir-group "Search server")
       ("g" gnus-group-get-new-news "Refresh")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose")
       ("#" gnus-topic-mark-topic "mark")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("s" gnus-summary-show-thread "Show")
       ("h" gnus-summary-hide-thread "Hide")
       ("n" gnus-summary-insert-new-articles "Refresh")
       ("f" gnus-summary-mail-forward "Forward")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all")
       ("e" gnus-summary-resend-message-edit "Resend")
       ("R" gnus-summary-reply-with-original "Reply with original")
       ("r" gnus-summary-reply "Reply")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original")
       ("w" gnus-summary-wide-reply "Reply all")
       ("m" gnus-uu-mark-thread "mark")
       ("d" gnus-summary-delete-article "delete")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original")
       ("r" gnus-article-reply "Reply")
       ("W" gnus-article-wide-reply-with-original "Reply all with original")
       ("o" gnus-mime-save-part "Save attachment at point")
       ("w" gnus-article-wide-reply "Reply all")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach")
       ("cc" message-send-and-exit "Send")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))

;;------------------------------------------------------------------------
;; DEFT
;;------------------------------------------------------------------------
(require 'deft)
(setq deft-directory "~/Documents/Organize/Notes"
      deft-archive-directory "~/Documents/Organize/Notes/Archive"
      deft-extensions '("org" "md" "txt")
      deft-default-extension "org"
      deft-archive-directory "Archive"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t)

;;------------------------------------------------------------------------
;; MARKDOWN
;;------------------------------------------------------------------------
(setq markdown-hide-markup t)
;; Add support for Lektor contents
(add-to-list 'auto-mode-alist '("\\.lr\\'" . markdown-mode))

;;------------------------------------------------------------------------
;; PYTHON
;;------------------------------------------------------------------------
(setq python-shell-interpreter "/usr/bin/python3")
(setenv "PYTHONPATH" "~/.local/lib/python3.7")
(add-to-list 'exec-path "~/.local/bin")
(add-hook 'python-mode-hook 'hl-line-mode)

;;------------------------------------------------------------------------
;; ANACONDA
;;------------------------------------------------------------------------
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; States for anaconda view
(add-to-list 'evil-emacs-state-modes 'anaconda-mode-view-mode)

;;------------------------------------------------------------------------
;; YAPF
;;------------------------------------------------------------------------
(add-hook 'python-mode-hook 'yapf-mode)

;;------------------------------------------------------------------------
;; HIGHLIGHT-INDENT-GUIDES
;;------------------------------------------------------------------------
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;;------------------------------------------------------------------------
;; ACE-WINDOW
;;------------------------------------------------------------------------
(setq aw-dispatch-always t)

;;------------------------------------------------------------------------
;; ENGINE
;;------------------------------------------------------------------------
(require 'engine-mode)
(engine-mode t)
;; Set default browser for engine
(setq engine/browser-function 'eww-browse-url)
;; Chane keymap
(engine/set-keymap-prefix (kbd "C-c s"))
;; Search on DuckDuckGo
(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")
;; Search on Google
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")
;; Search on GitHub
(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")
;; Search on Stack Overflow
(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")
;; Search on Wikipedia
(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

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
;; COMPANY
;;------------------------------------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)
;; No delay in showing suggestions.
(setq company-idle-delay 0)
;; Show suggestions after entering one character.
(setq company-minimum-prefix-length 1)
;; From bottom to top again
(setq company-selection-wrap-around t)
;; Use tab key to cycle through suggestions.
;; ('tng' means 'tab and go')
(company-tng-configure-default)

;;------------------------------------------------------------------------
;; EASY-HUGO
;;------------------------------------------------------------------------
(setq easy-hugo-basedir "~/Documents/Blog/Hugo/")
(setq easy-hugo-default-ext ".org")
(setq easy-hugo-postdir "content/blog")
;; States for easy-hugo
(add-to-list 'evil-emacs-state-modes 'easy-hugo-mode)

;;------------------------------------------------------------------------
;; EMOJIFY
;;------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-emojify-mode)

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
;; INSERT LINE
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
;; ISPELL SKIP FOR ORG
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
;; ORG ARROW FILTER TIMESTAMPS
;;------------------------------------------------------------------------
(add-to-list 'org-export-filter-timestamp-functions
             #'org-arrow-filter-timestamps)
(defun org-arrow-filter-timestamps (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    ((or `odt `html)
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))

;;------------------------------------------------------------------------
;; FIND EMACS INIT
;;------------------------------------------------------------------------
(defun find-emacs-init ()
  "Open init file in the current window."
  (interactive)
  (find-file user-init-file))

(defun find-emacs-dir ()
  "Open emacs directory in the current window."
  (interactive)
  (find-file user-emacs-directory))

;;------------------------------------------------------------------------
;; TOGGLE WINDOW SPLIT
;;------------------------------------------------------------------------
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; (define-key ctl-x-4-map "t" 'toggle-window-split)

;;------------------------------------------------------------------------
;; AUTO MOVE NEW WINDOW ADVISING
;;------------------------------------------------------------------------
;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------- MISCELLANEOUS  --------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))

;; No times for yes
(fset 'yes-or-no-p 'y-or-n-p)

;; Load secret weapon
;; (load-file "~/.secretweapon.el")

;; Magical closing parens
(electric-pair-mode 1)

;; Highlight current parens
(show-paren-mode 1)
(setq show-paren-delay 0)


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


