(with-eval-after-load "semantic"
  (require 'project)
  
  (defun semanticdb-analyze-project-files ()
    "Scan all project files for semantic tags.
`global-semanticdb-minor-mode' should already be on."
    (interactive)
    (let* ((count 0)
           (pr (project-current))
           (dirs (append
                  (project-roots pr)
                  (project-external-roots pr)))
           (files (project-file-completion-table pr dirs))
           (report (make-progress-reporter "Analysing files..." 0 (length files))))
      (dolist (file files)
        ;; (message file) ;; DEBUG
        (with-demoted-errors
            (semanticdb-file-table-object file))
        ;; (unless (find-buffer-visiting file)
        ;;   (let ((buf (find-file-noselect file)))
        ;;     (semantic-fetch-tags)
        ;;     (kill-buffer buf))))
        (progress-reporter-update report (setq count (1+ count))))
      (semanticdb-save-all-db)
      (progress-reporter-done report)))
  
  (setq semantic-default-submodes '(global-semantic-highlight-func-mode
                                    global-semantic-idle-local-symbol-highlight-mode
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-idle-summary-mode
                                    global-semanticdb-minor-mode))
  
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  (setq semantic-idle-work-update-headers-flag t)
  (setq semantic-lex-spp-use-headers-flag t)

  (setq semanticdb-project-root-functions
        (list
         (lambda (directory) (project-roots (project-current nil directory)))
         (lambda (directory) (locate-dominating-file directory ".dir-locals.el"))))
  
  ;; semantic imenu
  (setq semantic-imenu-adopt-external-members nil) ;; put class members on t he first imenu level
  (setq semantic-imenu-bucketize-file nil)         ;; don't use buckets
  ;; (setq semantic-imenu-expand-type-members nil)
  ;; (setq semantic-imenu-index-directory t) ;; index the entire directory for tags
  (setq semantic-imenu-summary-function 'semantic-format-tag-canonical-name) ;; see semantic-format-tag-functions

  (semantic-mode)

  ;; c.el
  )
