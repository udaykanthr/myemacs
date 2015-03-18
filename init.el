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
;;

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

;; ----------------------------------------------------------------------
;; Define automatic mode detection for file types.
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

                ;; SQL mode.
                ;; ("\\.mysql$" . sql-mode)

                ;; Configuration files.
                ("\\(?:\\.gitconfig\\|\\.gitmodules\\|config\\)$" . conf-mode)
                )
              auto-mode-alist))




