;;; Package --- Summary

;;; Commentary:
;; The main entry point into MIKEMACS. It either loads the pre-compiled MIKEMACS configuration file
;; or tangles and loades the MIKEMACS literate org configuration file.

;;; Code:

;; Let's start emacs up quietly.
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      mode-line-format nil)


(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "mikemacs.elc" user-emacs-directory))
      (load-file (expand-file-name "mikemacs.elc" user-emacs-directory))
    (require 'org)
    (org-babel-load-file (expand-file-name "mikemacs.org" user-emacs-directory))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode which-key wgrep web-mode web-beautify visual-fill-column vimrc-mode vi-tilde-fringe use-package tuareg toml-mode toc-org tide swift-mode stylus-mode stripe-buffer smex smartparens slime shrink-path shm shader-mode sass-mode rjsx-mode rainbow-mode rainbow-delimiters racer purescript-mode pug-mode psc-ide play-routes-mode pip-requirements phpunit php-refactor-mode php-boris persp-mode perl6-mode ox-reveal org-tree-slide org-plus-contrib org-bullets opencl-mode omnisharp ob-translate ob-sql-mode ob-rust ob-restclient nodejs-repl nlinum-hl neotree nav-flash nasm-mode mu4e-maildirs-extension moonscript modern-cpp-font-lock mips-mode merlin meghanada markdown-toc magithub julia-mode json-mode js2-refactor ivy-hydra irony-eldoc imenu-list imenu-anywhere hy-mode hl-todo highlight-quoted highlight-numbers highlight-indentation help-fns+ haxor-mode groovy-mode gorepl-mode golden-ratio go-guru go-eldoc gitignore-mode gitconfig-mode git-timemachine git-link git-gutter-fringe gist flycheck-rust flycheck-pos-tip flycheck-plantuml flycheck-perl6 flycheck-irony flycheck-elm evil-visualstar evil-textobj-anyblock evil-terminal-cursor-changer evil-snipe evil-mc evil-matchit evil-magit evil-iedit-state evil-goggles evil-escape evil-embrace evil-easymotion evil-commentary evil-args evil-anzu ensime emmet-mode elm-mode elixir-mode eldoc-eval dumb-jump doom-themes dockerfile-mode disaster dired-k demangle-mode dante cuda-mode counsel-projectile counsel-dash company-web company-tern company-statistics company-sourcekit company-shell company-restclient company-racer company-quickhelp company-php company-lua company-irony-c-headers company-irony company-go company-glsl company-ghc company-dict company-anaconda command-log-mode coffee-mode cmake-mode clean-aindent-mode cider centered-window-mode android-mode ace-window ace-link))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#ECBE7B"))))
 '(evil-goggles-delete-face ((t (:foreground "#ff6c6b"))))
 '(evil-goggles-paste-face ((t (:foreground "#98be65"))))
 '(evil-goggles-undo-redo-add-face ((t (:foreground "#98be65"))))
 '(evil-goggles-undo-redo-change-face ((t (:foreground "#51afef"))))
 '(evil-goggles-undo-redo-remove-face ((t (:foreground "#ff6c6b"))))
 '(evil-goggles-yank-face ((t (:foreground "#51afef"))))
 '(git-gutter+-modified ((t (:foreground "#ECBE7B"))))
 '(git-gutter-fr:modified ((t (:foreground "#ECBE7B"))))
 '(git-gutter:modified ((t (:foreground "#ECBE7B"))))
 '(org-level-1 ((t :foreground "#51afef" :inherit nil :height 1.2)))
 '(org-level-2 ((t :foreground "#DCAEEA" :inherit nil :height 1.1)))
 '(org-level-3 ((t :foreground "#a9a1e1" :inherit nil :height 1.1)))
 '(org-level-4 ((t :foreground "#ECBE7B" :inherit nil :height 1.1)))
 '(org-level-5 ((t :foreground "#46D9FF" :inherit nil :height 1.1)))
 '(smerge-refined-added ((t (:inherit (quote smerge-other)))))
 '(smerge-refined-removed ((t (:inherit (quote smerge-mine))))))
