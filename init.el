;;; init.el -- Emacs configuration in a single file.
;;
;; Copyright (C) 2015 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; NOTE: most of the code is picked from my favorite repo:
;;  Gemacs: https://github.com/gorakhargosh/gemacs , by - yesudeep@google.com (Yesudeep Mangalapilly)
;;
;;
;; Author: udaykanth.rapeti@gmail.com (Udaykanth R)
;;
;;; This file is NOT a part of GNU Emacs.
;;; The goal of this repo is to install emacs plugins without using package installers (Melpa, elget)
;;; as some corp networks are not permitted to access those packages installer links.
;; change test

(set-locale-environment "en_US.UTF-8")


;; ======================================================================
;; Initial configuration.
;; ======================================================================
;; (setq user-emacs-directory (file-name-directory (or (buffer-file-name)
;;                                           load-file-name)))
;;(add-to-list 'load-path user-emacs-directory)

;; Directory paths.
(setq mylisp-dir (expand-file-name
                  (concat user-emacs-directory "mylisp")))
;; Directory paths.
(setq site-lisp-dir (expand-file-name
                          (concat user-emacs-directory "site-lisp")))

;; Load-path.
(add-to-list 'load-path mylisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add all subdirectories of site-lisp to load path.
(let ((default-directory mylisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Add all subdirectories of site-lisp to load path.
(let ((default-directory site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; ;; Network-specific configuration is loaded from goog-network-dir.
;; (setq goog-network-name "corp.google.com"
;;       goog-network-dir (format "~/.%s/emacs.d/" goog-network-name)
;;       goog-network-re ".*[.]corp[.]google[.]com")

;; ======================================================================
;; Platform detection.
;; ======================================================================
(defun goog/platform/is-darwin-p ()
  "Determines whether the system is darwin-based (Mac OS X)"
  (interactive)
  (string-equal system-type "darwin"))


(defun goog/platform/is-linux-p ()
  "Determines whether the system is GNU/Linux-based."
  (interactive)
  (string-equal system-type "gnu/linux"))




(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.


(require 'goog-defuns)
(require 'gemacs-appearance)
(require 'gemacs-keyboard)
(require 'gemacs-markdown)
(require 'gemacs-git)
(require 'gemacs-workspace)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;(require 'gemacs-editing)

(add-to-list 'load-path
              "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;    (add-to-list 'load-path "~/Emacs/emmet/")
   (require 'emmet-mode)
   (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
   (add-hook 'html-mode-hook 'emmet-mode)
   (add-hook 'css-mode-hook  'emmet-mode)


(require 'expand-region)
(global-set-key (kbd "M-8") 'er/expand-region)

(require 'js2-mode)

;; (require 'ajc-java-complete-config)
;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
;; (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)
;; read ajc-java-complete-config.el  for more info .
;; (require 's)
;; (require 'eclim)
;; (global-eclim-mode)

;; (custom-set-variables
;;   '(eclim-eclipse-dirs '("~/Downloads/sts-bundle/sts-3.7.0.RELEASE"))
;;   '(eclim-executable "~/Downloads/sts-bundle/sts-3.7.0.RELEASE/eclim")
;;   )

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

;; (require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)

;; (require 'eclimd)


(require 'fill-column-indicator)
;; (setq fci-rule-color "grey")
(setq fci-rule-width 2)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(setq fci-rule-column 80)

;; (require 'highlight-80+)
;; ;; (add-hook fundamental-mode-hook (lambda () (highlight-80+-mode 1)))
;; (add-hook 'text-mode-hook 'highlight-80+-mode)
;; (progn
;;   (highlight-80+-mode t)
;;   )


;; (define-globalized-minor-mode highlight-80+-mode  highlight-80+-mode highlight-80+-mode)
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-lines-matching-regexp ".\{81\}" "hi-green-b")))
;; ----------------------------------------------------------------------
;; define automatic mode detection for file types.
;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ;; YAML.
                ("\\.raml" . yaml-mode)
                ("\\.yaml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)

                ;; CSS.
                ("\\.gss" . css-mode)

                ;; Sass.
                ("\\.sass" . sass-mode)
                ("\\.scss" . sass-mode)

                ;; HTML.
                ("\\.tmpl" . html-mode)  ;; Server-side template extension.
                ("\\.mustache" . html-mode)  ;; Mustache template extension.
                ("\\.ng" . html-mode)    ;; Angular templates.

                ;; Dart lang.
                ("\\.dart$" . dart-mode)

                ;; Python.
                ("\\BUCK$" . python-mode)
                ("\\BUILD$" . python-mode)
                ("\\SConscript" . python-mode)
                ("\\SConstruct" . python-mode)
                ("\\wscript$" . python-mode)

                ;; JavaScript.
                ("\\.js$" . js2-mode)
                ("\\.json$" . js2-mode)

                ;; Less CSS mode.
                ("\\.less\\'" . less-css-mode)

                ;; Markdown
                ("\\.text\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)
                ("\\.md\\'" . markdown-mode)

                ("\\.c\\'" . c-mode)
                ;; SQL mode.
                ;; ("\\.mysql$" . sql-mode)

                ;; Configuration files.
                ("\\(?:\\.gitconfig\\|\\.gitmodules\\|config\\)$" . conf-mode)
                )
              auto-mode-alist))



(require 'smtpmail)
(require 'starttls)

(setq message-send-mail-function 'smtpmail-send-it)
(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-use-gnutls t)
(setq smtpmail-stream-type 'starttls)
(setq starttls-extra-arguments '("--priority" "NORMAL:%COMPAT"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 ;; (setenv "MAILHOST" "pop.gmail.com")
 ;; (setq rmail-primary-inbox-list '("pop://udaykanth.rapeti:cdqkgujmqefsstgp@pop.gmail.com")
 ;;       rmail-pop-password-required t)

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))
