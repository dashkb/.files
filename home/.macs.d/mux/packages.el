;;; packages.el --- mux Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq mux-packages
    '(
      emamux
      ))

;; List of packages to exclude.
(setq mux-excluded-packages '())


(defun current-line ()
  (1+ (count-lines 1 (point))))

(defun file-and-line ()
  (format "%s:%d" (buffer-file-name) (current-line)))

(defun mux/ensure-emamux-initialized ()
  (or (emamux:set-parameters-p) (emamux:set-parameters)))

(defun mux/reinit ()
  (interactive)
  (emamux:set-parameters))

(defun mux/save-and-send-keys (keys)
  (save-buffer)
  (emamux:send-keys keys))

(defun mux/run-this-spec ()
  (interactive)
  (mux/ensure-emamux-initialized)
  (mux/save-and-send-keys (format "eval ${TEST_CMD} %s" (file-and-line))))

(defun mux/!! ()
  (interactive)
  (mux/ensure-emamux-initialized)
  (mux/save-and-send-keys"!!"))

(defun bind-emamux-keys ()
  (require 'emamux)
  (evil-leader/set-key "ors" 'mux/run-this-spec)
  (evil-leader/set-key "o!" 'mux/!!)
  (evil-leader/set-key "omv" 'mux/reinit)

  )

(defun mux/init-emamux ()
  (use-package emamux
    :defer t
    :init (bind-emamux-keys)))
