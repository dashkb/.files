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
  (evil-leader/set-key "oo[" 'org-agenda-file-to-front)
  (evil-leader/set-key "oo]" 'org-remove-file)
  (evil-leader/set-key "ooa" 'org-agenda)
  (evil-leader/set-key "ooca" 'org-capture)
  (evil-leader/set-key "oocf" 'org-capture-finalize)
  (evil-leader/set-key "oocr" 'org-capture-refile)
  (evil-leader/set-key "oock" 'org-capture-kill)
  (evil-leader/set-key "oor" 'org-refile)
  (evil-leader/set-key "wo" 'delete-other-windows)
  (evil-leader/set-key "oag" 'helm-projectile-pt)
  (evil-leader/set-key "ocro" 'crosshairs-mode)
  (evil-leader/set-key "oal" 'align-regexp)

  (spacemacs/set-leader-keys
    "gr" 'magit-rebase-popup
    "gR" 'magit-reset-popup
    "orr" 'rails-refresh
    "orc" 'clj-system-refresh
    "opr" 'stuff/remote-pry))

(defun stuff/lisp-stuff ()
  (interactive)
  (add-hook 'clojure-mode #'evil-cleverparens-mode)

  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")

  )

(defun stuff/org-stuff ()
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

  (require 'org-install)
  (require 'org-mobile)
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")))

  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/things.org")))

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
  )
