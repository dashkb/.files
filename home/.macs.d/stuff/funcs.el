(defun clj-system-refresh ()
  (interactive)
  (cider-interactive-eval "(require 'system.repl) (system.repl/reset)"))

(defun rails-refresh ()
  (interactive)
  (robe-rails-refresh))

(defun stuff/set-indent ()
  ((lambda (n)
     (setq-default evil-shift-width n)
     (setq-default js2-basic-offset n)
     (setq-default js2-mode-show-parse-errors nil)
     (setq-default js2-mode-show-strict-warnings nil)
     (setq-default javascript-indent-level n)
     (setq-default js-indent-level n)
     (setq-default tab-width n)
     (setq-default css-indent-offset n)
     (setq-default sh-indentation n)
     (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
     (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
     (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file

     (setq-default truncate-lines t)
     ) 2))

(defun stuff/remote-pry (&rest args)
  (interactive)
  (let ((buffer (apply 'make-comint "pry-remote" "pry-remote" nil args)))
    (switch-to-buffer buffer)
    (setq-local comint-process-echoes t)))

(defun stuff/set-mouse-color ()
  (interactive)
  (set-mouse-color "light gray"))


(defun stuff/set-some-keys ()
  (interactive)

  (spacemacs/set-leader-keys
    "gr" 'magit-rebase-popup
    "gR" 'magit-reset-popup
    "orr" 'rails-refresh
    "orc" 'clj-system-refresh
    "opr" 'stuff/remote-pry
    "ow[" 'paredit-wrap-square
    "ow{" 'paredit-wrap-curly
    "owr" 'paredit-wrap-round
    "ow(" 'paredit-wrap-round
    "o!" 'async-shell-command
    "wo" 'delete-other-windows
    "omc" 'stuff/set-mouse-color
    "oal" 'align-regexp
    "ocro" 'crosshairs-mode
    "oag" 'helm-projectile-pt
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
    (add-hook 'cider-repl-mode-hook cider-sp)

    )

  (ignore-errors (load (expand-file-name "~/quicklisp/slime-helper.el")))
  (setq-default inferior-lisp-program "sbcl")
  (setq-default geiser-chez-binary "chez")
  )

(defun stuff/fix-word-chars ()
  (interactive)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
  )

(defun stuff/org-stuff ()
  (interactive)
  (setq-default org-refile-use-outline-path 'file)
  (setq-default org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (require 'org-install)
  (require 'org-mobile)
  (setq-default org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")))

  (setq-default org-directory "~/org")
  (setq-default org-default-notes-file (concat org-directory "/things.org"))

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
  (setq-default browse-url-browser-function 'browse-url-chrome))

(defun stuff/js-stuff ()
  (interactive)

  (add-hook 'js2-mode-hook (lambda ()
                             (js2-mode-hide-warnings-and-errors))))

(defun stuff/ruby-stuff ()
  (interactive)
  (setq-default ruby-use-smie t)
  (setq-default enh-ruby-preserve-indent-in-heredocs t)
  )

(defun stuff/init-my-stuff ()
  (interactive)
  (setq-default system-uses-terminfo nil)

  (require 'evil-tmux-navigator)
  (evil-tmux-navigator-bind-keys)

  ;; (when (not (display-graphic-p))
  ;;   (load-file "~/.emacs.d/xclip.el")
  ;;   (require 'xclip)
  ;;   (turn-on-xclip))

  (setq-default helm-mode-fuzzy-match t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq-default clojure-align-forms-automatically t)

  (spacemacs/toggle-smartparens-globally-on)
  (spacemacs/toggle-centered-point-globally-on)

  (evil-global-set-key 'insert (kbd "C-k") 'yas-expand)

  (setq-default evil-escape-key-sequence "jk")

  (setq-default vc-follow-symlinks t)
  (setq-default projectile-switch-project-action 'magit-status)

  (stuff/org-stuff)
  (stuff/set-some-keys)
  (stuff/set-indent)
  (stuff/lisp-stuff)
  (stuff/fix-word-chars)
  (stuff/ruby-stuff)
  (stuff/set-default-browser)
  ;; (stuff/js-stuff)

  (message "Initialized stuff")
  )
