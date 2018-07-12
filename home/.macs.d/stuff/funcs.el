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
     (setq-default js2-pretty-multiline-declarations 'all)
     (setq-default javascript-indent-level n)
     (setq-default js-indent-level n)
     (setq-default tab-width n)
     (setq-default css-indent-offset n)
     (setq-default sh-indentation n)
     (setq-default sh-basic-offset n)
     (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
     (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
     (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
     (setq-default emmet-indentation n)
     (setq-default groovy-indent-offset n)

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

(defun stuff/lisp-stuff ()
  (interactive)

  (let* ((sp (lambda ()
               (smartparens-mode 1)
               (smartparens-strict-mode 1)
               (evil-cleverparens-mode 1)))
         (cider-sp (lambda ()
                      (sp)
                      (cider-repl-toggle-pretty-printing))))
    (add-hook 'emacs-lisp-mode sp)
    (add-hook 'clojure-mode-hook sp)
    (add-hook 'geiser-repl-mode-hook sp)
    (add-hook 'racket-mode-hook sp)
    (add-hook 'cider-repl-mode-hook cider-sp)
    )

  (ignore-errors (load (expand-file-name "~/quicklisp/slime-helper.el")))
  (setq-default inferior-lisp-program "sbcl")
  (setq-default geiser-chez-binary "chez")
  (setq-default racket-indent-sequence-depth 2)
  )

(defun stuff/fix-word-chars ()
  (interactive)
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
  )

(defun stuff/find-project-root ()
  (locate-dominating-file
   (or (buffer-file-name) default-directory)
   ".git"))

(defun stuff/exec-at-root (cmd)
  "Run a shell command at the project root"
  (interactive)
  (let ((default-directory (stuff/find-project-root)))
    (async-shell-command cmd)))

(defun stuff/use-eslint-from-node-modules ()
  (interactive)
  (let* ((root (stuff/find-project-root))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

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

  (evil-leader/set-key "fem" 'stuff/edit-this-file)
  (evil-leader/set-key "ogn" 'stuff/open-org-notes)
  )

(defun stuff/open-org-notes ()
  "opens the org-default-notes-file"
  (interactive)

  (find-file-existing org-default-notes-file))

(defun stuff/edit-this-file ()
  "edits this file"
  (interactive)

  (find-file-existing "~/.macs.d/stuff/funcs.el"))

(defun stuff/set-default-browser ()
  (interactive)
  (setq-default browse-url-browser-function 'browse-url-chrome))

(defun stuff/js-stuff ()
  (interactive)

  (add-hook 'js2-mode-hook (lambda ()
                             (stuff/use-eslint-from-node-modules)
                             (js2-mode-hide-warnings-and-errors)
                             )))

(defun stuff/ruby-stuff ()
  (interactive)
  (setq-default ruby-use-smie t)
  (setq-default enh-ruby-preserve-indent-in-heredocs t)
  (setq-default enh-ruby-deep-indent-construct nil)
  (setq-default enh-ruby-check-syntax nil)

  )

(defun stuff/db-redo ()
  (interactive)
  (stuff/exec-at-root "rake db:redo")
  (evil-window-prev 1))

(defun stuff/db-redo-for-demo ()
  (interactive)
  (stuff/exec-at-root "SEED_FOR_DEMO=true rake db:redo")
  (evil-window-prev 1))

(defun stuff/rails-cache-clear ()
  (interactive)
  (stuff/exec-at-root "rake cache:clear")
  (evil-window-prev 1))

(defun stuff/kill-buffers-i-dont-like ()
  (interactive)

  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))

    (kill-matching-buffers "shell")))

(defun stuff/replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

(defun stuff/bind-keys ()
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
    "o'" 'multi-term
    "wo" 'delete-other-windows
    "omc" 'stuff/set-mouse-color
    "oal" 'align-regexp
    "ocro" 'crosshairs-mode
    "oms" 'stuff/init-my-stuff
    "oag" 'helm-projectile-pt
    "po" 'helm-projectile-find-other-file
    "pt" 'projectile-multi-term-in-root
    "onf" 'stuff/name-foreman-buffer
    "ogg" 'stuff/switch-to-gemfile-buffer
    "ogf" 'stuff/switch-to-foreman-buffer
    "obr" 'stuff/kill-and-reopen-file
    "odb" 'stuff/db-redo
    "oddb" 'stuff/db-redo-for-demo
    "occ" 'stuff/rails-cache-clear
    "okb" 'stuff/kill-buffers-i-dont-like
    ))

(defun stuff/send-C-r ()
  (interactive)
  (term-send-raw-string "\C-r"))

(defun stuff/send-C-p ()
  (interactive)
  (term-send-raw-string "\C-p"))

(defun stuff/send-C-n ()
  (interactive)
  (term-send-raw-string "\C-n"))

(defun stuff/setup-term-mode ()
  (interactive)

  (evil-local-set-key 'insert (kbd "C-r") 'stuff/send-C-r)
  (evil-local-set-key 'insert (kbd "C-p") 'stuff/send-C-p)
  (evil-local-set-key 'insert (kbd "C-n") 'stuff/send-C-n)

  (message "Setting up term mode"))

(defun stuff/name-foreman-buffer ()
  (interactive)
  (rename-buffer "foreman"))

(defun stuff/switch-to-foreman-buffer ()
  (interactive)
  (switch-to-buffer "foreman"))

(defun stuff/switch-to-gemfile-buffer ()
  (interactive)
  (switch-to-buffer "Gemfile"))

(defun stuff/buffer-stuff ()

  (defvar killed-file-list nil
    "List of recently killed files.")

  (defun add-file-to-killed-file-list ()
    "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
    (when buffer-file-name
      (push buffer-file-name killed-file-list)))

  (add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

  (defun reopen-killed-file ()
    "Reopen the most recently killed file, if one exists."
    (interactive)
    (when killed-file-list
      (find-file (pop killed-file-list))))

  (defun stuff/kill-and-reopen-file ()
    (interactive)
    (kill-buffer)
    (reopen-killed-file)))

(defun stuff/init-my-stuff ()
  (interactive)
  (setq-default system-uses-terminfo nil)

  (ignore-errors
    (require 'evil-tmux-navigator)
    (evil-tmux-navigator-bind-keys))

  ;; (when (not (display-graphic-p))
  ;;   (load-file "~/.emacs.d/xclip.el")
  ;;   (require 'xclip)
  ;;   (turn-on-xclip))

  (setq-default helm-mode-fuzzy-match t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq-default clojure-align-forms-automatically t)

  ;; (spacemacs/toggle-smartparens-globally-on)
  ;; (spacemacs/toggle-centered-point-globally-on)

  (evil-global-set-key 'insert (kbd "C-k") 'yas-expand)

  (setq-default evil-escape-key-sequence "fd")
  (setq-default evil-escape-delay 0.5)

  (setq-default vc-follow-symlinks t)

  (add-to-list 'projectile-other-file-alist '("js" "scss"))
  (add-to-list 'projectile-other-file-alist '("scss" "js"))

  (setq-default projectile-switch-project-action 'magit-status)

  ; kill buffer with running process w/o confirmation step
  (setq-default kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ; don't auto-yank visual region
  (fset 'evil-visual-update-x-selection 'ignore)

  (add-hook 'after-change-major-mode-hook (lambda ()
                                            (if (not (equal major-mode 'term-mode))
                                                (spacemacs/toggle-centered-point-on)
                                              (stuff/setup-term-mode))))

  (add-to-list 'auto-mode-alist '("\\.cssm\\'" . css-mode))

  (setq create-lockfiles nil)

  (stuff/bind-keys)
  (stuff/org-stuff)
  (stuff/set-indent)
  (stuff/lisp-stuff)
  (stuff/fix-word-chars)
  (stuff/ruby-stuff)
  (stuff/set-default-browser)
  (stuff/buffer-stuff)
  (stuff/js-stuff)

  (stuff/set-mouse-color)

  (message "Initialized stuff"))
