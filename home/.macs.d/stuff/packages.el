;;; packages.el --- stuff layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Kyle Brett <kyle@hostname>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(defconst stuff-packages
  '(gist
    github-browse-file
    git-link
    ))

;; (defun stuff/init-exec-path-from-shell ()

;;   (use-package exec-path-from-shell
;;     :defer t
;;     :init
;;     (when window-system
;;       (message "doing the thing")
;;       (exec-path-from-shell-initialize))))
