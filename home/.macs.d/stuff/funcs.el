(defun clj-system-refresh ()
  (interactive)
  (cider-interactive-eval "(require 'system.repl) (system.repl/reset)"))

(defun rails-refresh ()
  (interactive)
  (robe-rails-refresh))

(defun stuff/set-indent ()
  ((lambda (n)
     (setq coffee-tab-width n) ; coffeescript
     (setq javascript-indent-level n) ; javascript-mode
     (setq js-indent-level n) ; js-mode
     (setq js2-basic-offset n) ; js2-mode
     (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
     (setq web-mode-css-indent-offset n) ; web-mode, css in html file
     (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
     (setq css-indent-offset n) ; css-mode
     ) 2))

(defun stuff/remote-pry (&rest args)
  (interactive)
  (let ((buffer (apply 'make-comint "pry-remote" "pry-remote" nil args)))
    (switch-to-buffer buffer)
    (setq-local comint-process-echoes t)))


(defun stuff/set-some-keys ()
  (interactive)
  (evil-leader/set-key "wo" 'delete-other-windows)
  (evil-leader/set-key "oag" 'helm-projectile-pt)
  (evil-leader/set-key "ocro" 'crosshairs-mode)
  (evil-leader/set-key "oal" 'align-regexp)

  (spacemacs/set-leader-keys
    "gr" 'magit-rebase-popup
    "gR" 'magit-reset-popup
    "orr" 'rails-refresh
    "orc" 'clj-system-refresh
    "opr" 'stuff/remote-pry
    "tep" 'evil-smartparens-mode
    "ow[" 'paredit-wrap-square
    "ow{" 'paredit-wrap-curly
    "owr" 'paredit-wrap-round
    "ow(" 'paredit-wrap-round
    ))


(defun stuff/lisp-stuff ()
  (interactive)

  (let* ((sp (lambda ()
               (smartparens-mode 1)
               (smartparens-strict-mode 1)
               (evil-smartparens-mode 1)))
         (cider-sp (lambda ()
                      (sp)
                      (cider-repl-toggle-pretty-printing))))
    (add-hook 'clojure-mode-hook sp)
    (add-hook 'geiser-repl-mode-hook sp)
    (add-hook 'racket-mode-hook sp)
    (add-hook 'cider-repl-mode-hook cider-sp))



  (ignore-errors (load (expand-file-name "~/quicklisp/slime-helper.el")))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")
  )

(defun stuff/fix-word-chars ()
  (interactive)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
  )

(defun stuff/org-stuff ()
  (interactive)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (require 'org-install)
  (require 'org-mobile)
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")))

  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/things.org"))

  (spacemacs/set-leader-keys-for-major-mode
    'org-mode
    "j" 'org-move-item-down
    "k" 'org-move-item-up)

  (evil-leader/set-key "oo[" 'org-agenda-file-to-front)
  (evil-leader/set-key "oo]" 'org-remove-file)
  (evil-leader/set-key "ooa" 'org-agenda)
  (evil-leader/set-key "ooca" 'org-capture)
  (evil-leader/set-key "oocf" 'org-capture-finalize)
  (evil-leader/set-key "oocr" 'org-capture-refile)
  (evil-leader/set-key "oock" 'org-capture-kill)
  (evil-leader/set-key "oor" 'org-refile)

  )

(defun stuff/set-default-browser ()
  (interactive)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome"))

(defun stuff/js-stuff ()
  (interactive)

  (add-hook 'js2-mode-hook (lambda ()
                             (js2-mode-hide-warnings-and-errors))))

(defun stuff/ruby-stuff ()
  (interactive)
  (setq ruby-use-smie t)
  )

(defun stuff/init-my-stuff ()
  (interactive)
  (setq system-uses-terminfo nil)

  (require 'evil-tmux-navigator)
  (evil-tmux-navigator-bind-keys)

  (when (not (display-graphic-p))
    (load-file "~/.emacs.d/xclip.el")
    (require 'xclip)
    (turn-on-xclip))

  (setq helm-mode-fuzzy-match t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq clojure-align-forms-automatically t)

  (stuff/org-stuff)
  (stuff/set-some-keys)
  (stuff/set-indent)
  (stuff/lisp-stuff)
  (stuff/fix-word-chars)
  (stuff/ruby-stuff)
  (stuff/set-default-browser)
  (stuff/js-stuff)
  )
