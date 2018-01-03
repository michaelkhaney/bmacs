;;; init.el --- Summary
;; Emacs configuration file.

;;; Commentary:

;; -----------------------------------------------------------------------------

;;; TODO: 
;; create bind for counsel-recentf
;; bind for paradox-upgrade-packages
;; bind for paradox-list-packages
;; configure paradox-execute-asynchronously

;;; Code:

;; Add package repositories.
(require 'package)
(package-initialize)
;; (require 'session)

(setq user-full-name "Michael Haney"
      user-mail-address "michaelkhaney@gmail.com")

;; ==================
;; PACKAGE MANAGEMENT
;; ==================

(add-to-list 'package-archives '("melpa"
             . "https://melpa.org/packages/") t)

;; Set default file encoding to utf-8.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; (require 'cl-lib)
;; Check for use-package; install if not present.
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)

;; (use-package try
;;   :ensure t)

;; Paradox.
(setq paradox-github-token "c5bcadada1cd7d2d1c721284b5103aee279c153b")

;; -----------------------------------------------------------------------------
;; Custom set variables.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" default)))
 '(global-vi-tilde-fringe-mode t)
 '(org-agenda-files
   (quote
    ("~/projects/notebook.org" "~/projects/qrtpcr_dmel_2l_gut/notebook.org" "~/Dropbox/org/gtd/gtd.org" "~/Dropbox/org/gtd/inbox.org")))
 '(package-selected-packages
   (quote
    (company-statistics async pcache yasnippet benchmark-init company-auctex company-bibtex company-plsense company-quickhelp company-shell company modeline-posn cyphejor diminish try smart-mode-line sml-mode sublimity pandoc-mode ox-pandoc pandoc all-the-icons auctex flycheck org-ref anti-zenburn-theme magit magit-annex rainbow-mode vlf org-babel-eval-in-repl ess ledger-mode esup zenburn-theme ws-butler which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets org open-junk-file nlinum neotree move-text macrostep lorem-ipsum link-hint ivy-hydra info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio flx-ido eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link)))
 '(paradox-automatically-star t)
 '(show-paren-mode t)
 '(sml/col-number-format "%2c")
 '(sml/modified-char "x")
 '(sml/numbers-separator ":")
 '(sml/position-percentage-format "%p")
 '(which-key-mode t))

;; -----------------------------------------------------------------------------
;; Customizing sml.
(setq-default sml/line-number-format " %3l")
(setq-default sml/mode-width 'full)
(setq-default sml/name-width 40)

;; -----------------------------------------------------------------------------

;; Company
(use-package company
  :config
  (progn
    (global-company-mode 1)
    (setq company-idle-delay 0.3)
    (setq company-show-numbers t)
    (setq company-minimum-prefix-length 2)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-other-buffers t)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq company-dabbrev-code-everywhere t)
    (setq company-dabbrev-code-ignore-case t)
    (global-set-key (kbd "C-<tab>") 'company-dabbrev)
    (global-set-key (kbd "M-<tab>") 'company-complete)
    (global-set-key (kbd "C-c C-y") 'company-yasnippet)))

(use-package company-quickhelp
  :config
  (progn
    (company-quickhelp-mode 1)))

;; -----------------------------------------------------------------------------

;; =====
;;; GUI
;; =====

;; Skip splash; goto scratch; remove scratch message.
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Hide menu-bar, tool-bar, and scroll-bar.
(if (featurep 'menu-bar) (menu-bar-mode -1))
(if (featurep 'tool-bar) (tool-bar-mode -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
;;(if (featurep 'fringe-bar) (set-fringe-mode 0))

;; Add vi tilde in fringe.
(global-vi-tilde-fringe-mode t)

;; Empty line markers at end of file.
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Modline.

;; (use-package cyphejor
;;   :ensure t
;;   :diminish cyphejor-mode "CY"
;;   :config
;;   (progn
;;     (setq cyphejor-rules
;;           '(:upcase
;;             ("bookmark"    "→")
;;             ("buffer"      "β")
;;             ("diff"        "Δ")
;;             ("dired"       "δ")
;;             ;; ("emacs"       "e")
;;             ;; ("emacs"       "ε")
;;             ("inferior"    "i" :prefix)
;;             ("interaction" "i" :prefix)
;;             ("interactive" "i" :prefix)
;;             ;; ("lisp"        "l" :postfix)
;;             ;; ("lisp"        "λ" :postfix)
;;             ("menu"        "▤" :postfix)
;;             ;; ("mode"        "m")
;; 	    ;; ("org"         "o")
;;             ("package"     "↓")
;;             ("python"      "py")
;;             ;; ("python"      "π")
;;             ("shell"       "sh" :postfix)
;;             ("text"        "ξ")
;;             ("wdired"      "↯δ")))
;;     (cyphejor-mode 1)))

;; Cyphejor.
(use-package cyphejor
  :ensure t
  :config
  (progn
    (setq cyphejor-rules
	  '(:upcase
      ("org" "O")
      ("mode" "")
	    ("emacs" "E")
	    ("lisp" "L")))
    (cyphejor-mode 1)))

;; (setq mode-line-position
;;             '(;; %p print percent of buffer above top of window, o Top, Bot or All
;;               ;; (-3 "%p")
;;               ;; %I print the size of the buffer, with kmG etc
;;               ;; (size-indication-mode ("/" (-4 "%I")))
;;               ;; " "
;;               ;; %l print the current line number
;;               ;; %c print the current column
;;               (line-number-mode ("%l" (column-number-mode ":%c")))))


(setq modelinepos-column-limit 80)


;; (setq-default mode-line-format
;;       '("%e"
;;         mode-line-front-space
;;         ;; mode-line-mule-info -- I'm always on utf-8
;;         mode-line-client
;;         mode-line-modified
;;         ;; mode-line-remote -- no need to indicate this specially
;;         ;; mode-line-frame-identification -- this is for text-mode emacs only
;;         " "
;;         mode-line-directory
;;         mode-line-buffer-identification
;;         " "
;;         mode-line-position
;;         ;;(vc-mode vc-mode)  -- I use magit, not vc-mode
;;         (flycheck-mode flycheck-mode-line)
;;         " "
;;         mode-line-modes
;;         mode-line-misc-info
;;         mode-line-end-spaces))

(line-number-mode t)
(column-number-mode t)

;; (setq-default mode-line-format '(" %e" mode-line-front-space mode-line-modified (vc-mode vc-mode) mode-line-frame-identification mode-line-buffer-identification "" mode-line-position mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; (setq-default mode-line-format '(" %e"))

;; smart-mode-line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(sml/setup)
;; Defined abbreviations.
;; (add-to-list 'sml/replacer-regexp-list '("^~/org/" ":o:") t)
;; (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs//.d/" ":ed:") t)
;; (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":p:") t)
;; (add-to-list 'sml/replacer-regexp-list '("^~/\\.config/" ":c:") t)
;; (add-to-list 'sml/replacer-regexp-list '("^:c:\\(.*\\)/i3/" ":c/\\1/i3:") t)

;; Supposedly better than line-number-at-pos; https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers
(string-to-number (format-mode-line "%l"))

;; (setq rm-blacklist
;;       (format "^ \\(%s\\)$"
;;               (mapconcat #'identify
;;                          '("Fly.*" "Projectile.*" "PgLn" "mkh")
;;                          "\\|")))

;; (setq-default mode-line-format
;;       (list
;;        ;; if edited or not
;;        " %&%&"
;;        ;; line number
;;        " [%l"
;;        ;; column number
;;        ":%c] "
;;        ;; percentage
;;        "%p "
;;        ;; name of file
;;        "%b "
;;        ;; major mode
;;        "%m "
;;        "%e"
;;        ))

;; Non Blinking Cursor.
(blink-cursor-mode 0)

;; Font.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11" ))
(set-face-attribute 'default nil :font "DejaVu Sans Mono-11" :height 110)  ; 11 font size.

;; Zenburn theme.
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Change "yes/no" responses for "y/n".
(fset 'yes-or-no-p 'y-or-n-p)

;; Unto tree mode.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  ;; :diminish unto-tree-mode "UT"
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))


;; Pop-to-mark - get back to previous places.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; Windmove - switching between windows.
(use-package windmove
  :bind
  (("<f2> l" . windmove-right)
   ("<f2> h" . windmove-left)
   ("<f2> k" . windmove-up)
   ("<f2> j" . windmove-down)))


;; View keystroke echo faster in modeline.
(setq echo-keystrokes 0.1
      ;; use-dialog-box nil
      visible-bell t)

;; Which-key.
(use-package which-key
  :defer t
  :diminish which-key-mode
  ;; :diminish which-key-mode "WK"
  :init
  (progn
    (setq which-key-paging-prefixes '("C-x" "C-c" "C-h"))
    (setq which-key-define-key-recursively t)
    (setq which-key-idle-delay 0.5)
    (setq which-key-max-description-length 27)
    (setq which-key-show-prefix 'left))
  :config
  (progn
    (which-key-mode)))


;; ;; Help - guide-key.
;; (use-package guide-key
;;   :defer t
;;   :diminish guide-key-mode "GK"
;;   :config
;;   (progn
;;     (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h"))
;;     (setq guide-key/recursive-key-sequence-flag t) ; Affects prefixes after.
;;     (setq guide-key/idle-delay 1.0)         ; Default is 1.0 sec.
;;     (setq guide-key/popup-window-position :bottom)
;;     (guide-key-mode 1)))


;; Rainbow mode
(rainbow-mode 1)
;; (diminish 'rainbow-mode' "RB")

;; Enable Highlight Line Mode
(global-hl-line-mode t)
(set-face-background hl-line-face "#4b4b4b")

;; Fill column.
(setq-default fill-column 80)

;; popwin - popup window manager
;; Use "C-g" to close popup windows
(require 'popwin)
(popwin-mode 1)

;; sublimity minimap
;; (autoload 'sublimity t)
;; (require 'sublimity)
;; (sublimity-mode 1)
;; (require 'sublimity-attractive)
;; (setq sublimity-map-size 20)


;; Flycheck
;; (global-flycheck-mode)
;; Disable flycheck for emacs-lisp
;; (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))


;; Move backup files to backup directory.
;; https://stackoverflow.com/questions/2680389/how-to-remove-all-files-ending-with-made-by-emacs
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t               ; Don't delink hardlinks.
      version-control t                 ; Use version numbers on backups.
      delete-old-versions t             ; Automatically delete excess backups.
      kept-new-versions 20              ; Keep <int> new versions.
      kept-old-versions 5               ; Keep <int> old versions.
)

;; =============================================================================
;; AUCTex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

;; =============================================================================
;; Language hooks.

;; Shell-script-mode for zsh files.
(add-to-list 'auto-mode-alist '("\\\\.zsh$" . shell-script-mode))

;; =============================================================================
;; Neotree
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)                 ;jump to file node when toggled
(setq neo-show-hidden-files t)          ;instead of using "H" for hidden files
;; -----------------------------------------------------------------------------

;; ===========================
;; Org SRC Blocks
;; ===========================

;; Auto revert all clients open in daemon when reverting buffer
(global-auto-revert-mode t)

;; Tabs.
;; Use 2 spaces instead of tabs.
(setq tab-width 2
      indent-tabs-mode nil)
;; (setq indent-tabs-mode nil)
;; (setq-default c-basic-offset 4)

;; Using ~C-c '~ to open window and edit src code blocks opens in same window.
(setq org-src-window-setup 'current-window)

;; SRC custom templates
;; ("s" "")

;; (add-to-list 'org-structure-template-alist
;; 	     (list "s" (concat "")))

;; Indent guide
(require 'indent-guide)
(indent-guide-global-mode)
;; (diminish 'indent-guide-mode "IN")

;; Background color
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(modelinepos-column-warning ((t (:inherit modelinepos-region))))
 '(org-block ((t (:background "#323232"))))
 '(org-block-begin-line ((t (:foreground "#709080" :background "#1e2320"))))
 '(org-block-end-line ((t (:foreground "#709080" :background "#1e2320"))))
 '(org-level-1 ((t :background "#181818")))
 '(org-level-2 ((t :background "#181818")))
 '(org-level-3 ((t :background "#181818")))
 '(org-level-4 ((t :background "#181818")))
 '(org-level-5 ((t :background "#181818")))
 '(org-level-6 ((t :background "#181818"))))

;; Org src blocks default folded
(add-hook 'org-mode-hook 'org-hide-block-all)

;; Fontify code in code blocks.
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)


;; Tag tasks with ~GTD contexts based on location.
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@coding" . ?c)
                      ("@reading" . ?r)
                      ("fuzzy" . ?0)
                      ("@errands" . ?e)
                      ("@phone" . ?p)
                      ("@email" . ?m)
                      ("@travel" . ?t)))

;; -----------------------------------------------------------------------------

;; ============
;; AGENDA/GTD
;; ============

(use-package org-install
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :mode ("\\.org$" . org-mode)
  :diminish (orgstruct-mode orgstruct++-mode orgtbl-mode))

;; Capture timestamps when TODO states change.
(setq org-log-done 'time)

;; Files.
(setq org-directory "~/org"
      org-default-notes-file "~/org/note.org")

;; ;; (setq org-directory "~/org")
;; ;; (setq org-default-notes-file "~/org/note.org")
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cc" 'org-capture)
;; (define-key global-map "\C-cl" 'org-store-link)
;; ;; Switch org files.
;; (define-key global-map "\C-cb" 'org-iswitchb)

;; (setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
;;                          "~/Dropbox/org/gtd/gtd.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")))
                              ;; ("T" "Tickler" entry
                              ;;  (file+headline "~/Dropbox/org/gtd/tickler.org" "Tickler")
                              ;;  "* %i%? \n %U")))

;; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; Refile targets.
(setq org-refile-targets '(("~/Dropbox/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/org/gtd/someday.org" :level . 1)))
                           ;; ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 2)))

;; References
;; Archive
;; Task states are untouched when archived (not auto DONE).
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

;; Fast todo selection via <C-c C-t KEY>.
(setq org-use-fast-todo-selection t)

;; TODO Keywords.
;; @/! toggle logging; @ prompts for note; ! auto logs timestamp.
(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
                        "WAIT(w@/!)"
                        "STARTED(s)"
                        "APPT(a)"
                        "SOMEDAY(.)" "|" "DONE(x!)" "CANCELED(c@)"))))

;; TODO colors.
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAIT" . (:foreground "red" :weight bold))
        ("CANCELED" . (:foreground "#f0dfaf" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))
        ("APPT" . (:foreground "#94bff3" :weight bold))))


;; ;; Tags added per state changes.
;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELED" ("CANCELED" . t))
;;               ("WAIT" ("WAIT" . t))
;;               ("HOLD" ("WAIT") ("HOLD" . t))
;;               ("DONE" ("WAIT") ("HOLD"))
;;               ("TODO" ("WAIT") ("CANCELED") ("HOLD"))
;;               ("NEXT" ("WAIT") ("CANCELED") ("HOLD"))
;;               ("DONE" ("WAIT") ("CANCELED") ("HOLD")))))

;; Agenda Custom Commands
(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
		  
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; -----------------------------------------------------------------------------

;; =============================================================================
;; org-ref
;; =============================================================================

;; (require 'org-ref)
(use-package org-ref)
(require 'org-ref-pubmed)

;; Set org-ref variables.
(setq reftex-default-bibliography
      '("~/projects/bibliography/references.bib"))
(setq org-ref-bibliography-notes "~/projects/bibliography/bibnotes.org"
      org-ref-default-bibliography '("~/projects/bibliography/references.bib")
      org-ref-pdf-directory "~/projects/bibliography/pdfs/")

;; Change keybinding to insert citation; issues with an org-mode command
(setq org-ref-insert-cite-key "C-c )")

;; Convienent keys for working in bibtex file
(setq bibtex-completion-bibliography "~/projects/bibliography/references.bib"
      bibtex-completion-library-path "~/projects/bibliography/pdfs"
      bibtex-completion-notes-path "~/projects/bibliography/bibtex_notes")

;; ivy as backend completion engine for searching and entering citations
(setq org-ref-completion-library 'org-ref-ivy-cite)

;; org-export
;; Set preferred output format to doc for odt export
(setq org-export-odt-preferred-output-format "doc")
(setq org-odt-preferred-output-format "doc")

;; Fix pandoc citation style
;; (defun helm-bibtex-format-pandoc-citation (keys)
;;   concat "[" (mapconcat (lambda (key) (concat "@" key)) keys "; ") "]")

;; ;; Inform helm-bibtex how to format citation in org-mode
;; (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions))
;;       'helm-bibtex-format-pandoc-citation)

;; -----------------------------------------------------------------------------

;; Package Configurations

;; -----------------------------------------------------------------------------

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Smartparens - auto insert pairs.
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (require 'smartparens-latex)
    (smartparens-global-mode t)
    (setq sp-autodelete-wrap t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq-default sp-autoskip-closing-pair t)
    (setq sp-autoescape-string-quote nil)))     ;don't escape quotes in strings

;; ----------------------------------------------------------------------------

;; ESS
;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(require 'ess-site)

(require 'ess-view)

;; -----------------------------------------------------------------------------
;; View Large Files (vlf)

;; Org display inline images
;; (defun do-org-show-all-inline-images()
;;   (interactive)
;;   (org-display-inline-images t t))
;; (global-set-key (kbd "C-c C-x C v")
;; 		'do-org-show-all-inline-images)

;; -----------------------------------------------------------------------------

;; Golden-ratio.
(use-package golden-ratio
  :ensure t
  :config
  (progn
    (setq golden-ratio-auto-scale 0
          golden-ratio-adjust-factor 1.0
          golden-ratio-exclude-modes '(imenu-list-major-mode
                                       eshell-mode
                                       ansi-term-mode
                                       pdf-view-mode
                                       mu4e-main-mode
                                       mu4e-headers-mode
                                       calendar-mode
                                       compilation-mode))
    (golden-ratio-mode 1)
    ;; (add-to-list 'golden-ratio-extra-commands
    ;;              '("ace-window"
    ;;                "avy-goto-char-2"
    ;;                "avy-goto-char-timer"))))
    ))

;; Exceptions for GR only work if added separately.
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'avy-goto-char-2)
(add-to-list 'golden-ratio-extra-commands 'avy-goto-char-timer)

  ;; :diminish golden-ratio-mode "GR"

;; -----------------------------------------------------------------------------

;; Ivy
;; (use-package ivy
;;   :ensure t
;;   :diminish IV
;;   :config
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     (setq ivy-display-style 'fancy)
;;     (setq ivy-count-format "(%d/%d) ")
;;     (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package swiper
  :ensure try
  :diminish 
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq ivy-count-format "(%d/%d) ")
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


;; Avy - jump visible text using a char-based decision tree.
(use-package avy
  :ensure t
  :config
  (progn
    (setq avy-background t)                   ;dark background
    (setq avy-highlight-first t)              ;used with avy-lead-face-0
    (setq avy-timeout-seconds 4.0))           ;timer function timeout
  :init
  (progn
    (global-set-key (kbd "M-g g") 'avy-goto-line)
    (global-set-key (kbd "M-g M-g") 'avy-goto-line)))



;; Ace-window
(use-package ace-window
  :ensure t
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq aw-background t)
    (setq aw-dispatch-always t)
    (ace-window-display-mode t))
  :init
  (progn
    (global-set-key (kbd "M-p") 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; -----------------------------------------------------------------------------

;; Replace list-buffers with ibuffer as default.
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config (progn
            ;; https://github.com/magnars/expand-region.el
            (defun er/add-text-mode-expansions ()
              (make-variable-buffer-local 'er/try-expand-list)
              (setq er/try-expand-list (append
                                        er/try-expand-list
                                        '(mark-paragraph
                                          mark-page)))
              (add-hook 'markdown-mode-hook 'er/add-text-mode-expansions)
              (add-hook 'LaTeX-mode-hook 'er/add-text-mode-expansions))))

;; Typing on a selected region replaces it.
(pending-delete-mode t)


;; Set default browser to Qutebrowser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; Require org and org-babel.
;; Use 'org-install over 'org; some org variables must be set before org.el is
;; loaded.
;; (require 'org-install)

;; Active Babel languages.
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (emacs-lisp . t)
   (python . t)
   (org . t)
   (latex . t)
   (java . t)
   (perl . t)
   (ditaa . t)))

;; Location of ditaa.jar file.
(setq org-ditaa-jar-path "/usr/share/java/ditaa-eps/DitaaEps.jar")

;; Remove confirmation for evaluating src blocks.
(setq org-confirm-babel-evaluate nil)

;; Customize interface. Use =M-x customize-group RET $package RET=
;; where $package is package of interest.


;; Use mkh minor mode for keybindings that work in all modes.
(defvar mkh-minor-mode-map (make-keymap) "mkh-minor-mode keymap.")
;; (define-key mkh-minor-mode-map (kbd "C-;") 'avy-goto-char-2)
(define-key mkh-minor-mode-map (kbd "C-;") 'avy-goto-char-timer)
;;(define-key mkh-minor-mode-map (kbd "C-;") 'avy-goto-char-timer)
;;(define-key mkh-minor-mode-map (kbd "C-c ;") 'avy-goto-char-timer)
(define-key mkh-minor-mode-map (kbd "C-c n") 'nlinum-mode)
(define-key mkh-minor-mode-map (kbd "C-c w") 'whitespace-mode)
(define-key mkh-minor-mode-map (kbd "M-p") 'ace-window)
(define-key mkh-minor-mode-map (kbd "C-c f") 'fci-mode)
(define-key mkh-minor-mode-map (kbd "C-c e") 'eval-buffer)
(define-key mkh-minor-mode-map (kbd "C-c r") 'rainbow-mode)
(define-key mkh-minor-mode-map (kbd "<f2> SPC g") 'which-key-mode)
;; Highlight and underline columns after 80.
(define-key mkh-minor-mode-map (kbd "C-c SPC c") 'column-enforce-mode)
;; Better word wrapping.
(define-key mkh-minor-mode-map (kbd "C-c SPC v") 'visual-line-mode)
(define-key mkh-minor-mode-map (kbd "<f2> SPC C-c c") 'company-quickhelp-mode)
(define-key mkh-minor-mode-map (kbd "<f2> SPC r") 'restart-emacs)
(define-key mkh-minor-mode-map (kbd "<f2> SPC e") 'counsel-recentf)
;; (define-key mkh-minor-mode-map (kbd "C-c s") 'smartparens-global-mode)
(define-minor-mode mkh-minor-mode
  "A minor mode so kbd's work in all major/minor modes"
  t " mkh" 'mkh-minor-mode-map)

;; (diminish 'mkh-minor-mode' "MH")


;; =============================================================================

;; (global-set-key (kbd "C-s") 'swiper)
;; (global-set-key (kbd "C-r") 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)

;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; ;; avy.el
;; ;; Darken background.
;; (setq avy-background t)
;; ;; Highlight first decision char with avy-lead-face-0.
;; (setq avy-highlight-first t)
;; ;; Timeout for timer functions.
;; (setq avy-timeout-seconds 5.0)

;; Select 1-2 chars and navigate to line. Number entry also available.
;; (global-set-key (kbd "M-g g") 'avy-goto-line)
;; (global-set-key (kbd "M-g M-g") 'avy-goto-line)

;; (global-set-key (kbd "M-p") 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; (ace-window-display-mode t)

;; (require 'smartparens-config)
;; (smartparens-global-mode t)


 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; '(org-block
 ;;   ((t (:background "#323232"))))
 ;; '(org-block-begin-line
 ;;   ((t (:underline "#dcdccc":foreground "#709080" :background "#1e2320"))))
 ;; '(org-block-end-line
 ;;   ((t (:overline "#dcdccc" :foreground "#709080" :background "#1e2320")))))

(setq debug-on-error t)


(provide 'init)
