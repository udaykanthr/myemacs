;;; gemacs-git.el --- Git related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Git integration.

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "../site-lisp/magit/")))
(require 'magit)

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load "magit"
  '(progn
     (setq magit-completing-read-function 'magit-ido-completing-read)
     ))

(provide 'gemacs-git)

;;; gemacs-git.el ends here
