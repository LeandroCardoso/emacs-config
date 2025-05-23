;;; teamcity.el --- Major mode for viewing TeamCity build logs -*- lexical-binding: t; -*-

;; Copyright (C) Leandro Cardoso

;; Maintainer: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;; This major mode provides syntax highlighting for TeamCity log files, typically generated by
;; TeamCity CI builds.
;;
;; Automatically activates for files containing "Plugins" or "Build" in their name and ending in
;; `.log`.

;;; Code:

(require 'generic)
(require 'mode-local)

(define-generic-mode teamcity-log-mode
  nil ;; No comment delimiters
  nil ;; No keywords
  '(("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] :" . compilation-info-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]F:" . font-lock-function-name-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]i:" . font-lock-keyword-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]W:" . compilation-warning-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]E:" . compilation-error-face)
    (" \\[.*\\] "                               . font-lock-comment-face))
  '("\\(Plugins\\|Build\\).*\\.log\\'") ;; Files to auto-enable this mode for
  nil)

;; Disable default string highlighting because TeamCity logs often contain malformed strings
(setq-mode-local teamcity-log-mode font-lock-keywords-only t)

(provide 'teamcity)

;;; teamcity.el ends here
