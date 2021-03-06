;;; gemacs-keyboard.el --- Keyboard related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Reload the user init file.
;; (global-set-key (kbd "C-.") 'goog/elisp/reload-configuration)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Open recent files using `ido-mode'.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Helm find files.
(global-set-key (kbd "M-F") 'helm-for-files)

;; Shift region left or right.
(global-set-key (kbd "s-]") 'shift-right)
(global-set-key (kbd "s-[") 'shift-left)
(global-set-key (kbd "RET") 'newline-and-indent)

;; ibuffer.
(global-set-key [(f8)] 'ibuffer)

;; Increase/decrease/reset font size.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-reset)

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Killing and yanking.
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-<delete>") 'kill-word)

;; Line insertion
(global-set-key (kbd "S-<return>") 'insert-blank-line-below)
(global-set-key (kbd "M-S-<return>") 'insert-blank-line-above)
(global-set-key (kbd "s-<return>") 'insert-blank-line-below-next-line)

;; Line or region duplication.
(global-set-key (kbd "C-x C-d") 'duplicate-current-line-or-region)

;; Toggle identifier case.
(global-set-key (kbd "C-x t c") 'toggle-identifier-naming-style)

;; Comment what I really mean.
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Sort lines
(global-set-key (kbd "C-c s") 'sort-lines)

;; Jump easily between beginning and end of defuns.
(global-set-key (kbd "s-<up>") 'beginning-of-defun)
(global-set-key (kbd "s-<down>") 'end-of-defun)

;; Parentheses matching.
(global-set-key (kbd "M-0") 'goto-match-paren)

;; Quotes.
(global-set-key (kbd "C-'") 'toggle-quotes)

;; Transpose parameters.
(global-set-key (kbd "s-t") 'transpose-params)

;; Perspectives.
;; NOTE(yesudeep): No longer functional.
;; (global-set-key (kbd "s-<left>") 'persp-prev)
;; (global-set-key (kbd "s-<right>") 'persp-next)

;; Helm.
(global-set-key (kbd "C-c h") 'helm-mini)

;; Switch buffer.
;; (global-set-key (kbd "C-x o") 'switch-window)

;; Evil numbers.
(global-set-key (kbd "M-_") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "M-+") 'evil-numbers/inc-at-pt)

;; I don't use F2 much, so binding it here to highlight symbol.
(global-set-key [(control f2)] 'highlight-symbol-at-point)
(global-set-key [f2] 'highlight-symbol-next)
(global-set-key [(shift f2)] 'highlight-symbol-prev)
(global-set-key [(meta f2)] 'highlight-symbol-prev)

;; Search, search, search.
(global-set-key [(f5)] 'ack-and-a-half-find-file)
(global-set-key [(f7)] 'ack-and-a-half)
(global-set-key [(shift f7)] 'ack-and-a-half-same)
;; (global-set-key [(shift f7)] 'multi-occur-in-this-mode) ;; Find in all buffers.
(global-set-key [(meta o)] 'ido-goto-symbol)     ;; Jump to symbol.

;; Transparency.
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Org mode key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; ----------------------------------------------------------------------
;; Key chords
;; ----------------------------------------------------------------------
;; Let the power to type multiple keys at the same time be yours.
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

(key-chord-define-global "hj" 'undo)
(key-chord-define-global "jk" 'dabbrev-expand)
(key-chord-define-global ";'" 'ido-recentf-open)
(key-chord-define-global ",." 'ido-find-file)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; company complete
(global-set-key (kbd "M-s TAB") 'company-complete)
(global-set-key (kbd "M-<f9>") 'recompile)
(global-set-key (kbd "C-<f9>") 'compile)

(provide 'gemacs-keyboard)

;;; gemacs-keyboard.el ends here
