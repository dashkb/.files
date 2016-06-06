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
  "gR" 'magit-reset-popup)

(defun kb/jk ()
  (interactive)
  (let* ((initial-key ?j)
         (final-key ?k)
         (timeout 0.25)
         (event (read-event nil nil timeout)))
    (if event
        ;; timeout met
        (if (and (characterp event) (= event final-key))
            (evil-normal-state)
          (insert initial-key)
          (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))

(define-key evil-insert-state-map (kbd "j") 'kb/jk)

(defun kb/indent-after-exit-insert-mode ()
  (add-hook 'evil-insert-state-exit-hook 'indent-according-to-mode nil t))

(add-hook 'enh-ruby-mode-hook 'kb/indent-after-exit-insert-mode)


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
(setq org-default-notes-file (concat org-directory "/things.org"))
(set-face-background 'col-highlight "color-19")

;; (set-face-attribute 'helm-selection nil 
;;                     ;; :background "purple"
;;                     :foreground "green")

((lambda (n)
   (setq coffee-tab-width n) ; coffeescript
   (setq javascript-indent-level n) ; javascript-mode
   (setq js-indent-level n) ; js-mode
   (setq js2-basic-offset n) ; js2-mode
   (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
   (setq web-mode-css-indent-offset n) ; web-mode, css in html file
   (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
   (setq css-indent-offset n) ; css-mode
   ) 2)
(setq system-uses-terminfo nil)

(require 'evil-tmux-navigator)
(evil-tmux-navigator-bind-keys)

(when (not (display-graphic-p))
  (xclip-mode 1))
(setq helm-mode-fuzzy-match t)
