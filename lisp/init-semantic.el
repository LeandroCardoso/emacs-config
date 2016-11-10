(with-eval-after-load "semantic"
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
         (lambda (directory) (locate-dominating-file directory ".git"))
         (lambda (directory) (locate-dominating-file directory ".tfignore"))
         (lambda (directory) (locate-dominating-file directory "view.dat"))
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
