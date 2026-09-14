;;; imenu-faces.el --- Add faces to imenu entries -*- lexical-binding: t; -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;; Adds faces to imenu prefixes and separators by advising
;; `imenu--flatten-index-alist'.

;;; Code:

(require 'imenu)

(defface imenu-prefix
  '((t :inherit font-lock-type-face))
  "Face used for imenu prefixes."
  :group 'imenu)

(defface imenu-level-separator
  '((t :inherit imenu-prefix :weight bold))
  "Face used for imenu level separators."
  :group 'imenu)

(defun imenu-faces--propertize-advice (func &rest args)
  (let ((imenu-level-separator
         (propertize imenu-level-separator 'face 'imenu-level-separator)))
    (when (stringp (nth 2 args))
      (setf (nth 2 args)
            (propertize (nth 2 args) 'face 'imenu-prefix)))
    (apply func args)))

(advice-add #'imenu--flatten-index-alist :around #'imenu-faces--propertize-advice)

(provide 'imenu-faces)

;;; imenu-faces.el ends here
