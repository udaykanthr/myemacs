;; gemacs-appearance.el --- Appearance related stuff.
;;
;;; Commentary:
;;
;;; Code:

(when window-system
  (blink-cursor-mode nil)
  (global-font-lock-mode 1)
  (global-hl-line-mode)
  (mouse-wheel-mode t)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )


(progn
  (column-number-mode 1)             ;; show column numbers in mode line.
  (delete-selection-mode 1)          ;; Overwrite selection; DWIM.
  (fset 'yes-or-no-p 'y-or-n-p)      ;; y or n is better than yes or no.
  (global-auto-revert-mode 1)        ;; Auto-reflect on-disk changes.
  (global-linum-mode 1)              ;; line numbers in left gutter.
  (global-subword-mode t)            ;; Use subword navigation.
  (line-number-mode 1)               ;; show line numbers in the mode line.
  )

(require 'zerodark-theme)
;; ;;Now set the default theme.              ;
;; (when window-system
;;   (require 'blackboard-theme)
;;   )

(require 'powerline)
(powerline-default-theme)
;; (zerodark-setup-modeline-format)


(provide 'gemacs-appearance)

;;; gemacs-appearance.el ends here
