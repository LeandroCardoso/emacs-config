;;; teamcity.el --- TeamCity log mode for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'generic)
(require 'mode-local)

(define-generic-mode teamcity-log-mode       ; MODE
  nil                                        ; COMMENT-LIST
  nil                                        ; KEYWORD-LIST
  '(("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] :" . compilation-info-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]F:" . font-lock-function-name-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]i:" . font-lock-keyword-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]W:" . compilation-warning-face)
    ("\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]E:" . compilation-error-face)
    (" \\[.*\\] " . font-lock-comment-face)) ; FONT-LOCK-LIST
  '("\\(Plugins\\|Build\\).*\\.log$")        ; AUTO-MODE-LIST
  nil)                                       ; FUNCTION-LIST

;; Ugly hack to disable automatic string highlight. This is disabled due to several malformed
;; strings.
(require 'mode-local)
(setq-mode-local teamcity-log-mode font-lock-keywords-only t)

(provide 'teamcity)

;;; teamcity.el ends here
