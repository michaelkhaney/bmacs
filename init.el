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
