(when (require 'plantuml-mode nil t)
  (setq plantuml-indent-level 4)

  (let ((plantuml-dir (expand-file-name "plantuml" user-emacs-directory)))
    (setq plantuml-jar-path (when (file-directory-p plantuml-dir)
                              (car (directory-files plantuml-dir t "plantuml.*jar"))))
    (setq plantuml-default-exec-mode (if plantuml-jar-path 'jar 'server)))

  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  ;; Workaround for fix plantuml server url encode. See
  ;; https://github.com/skuro/plantuml-mode/pull/172/commits/88ec2b989b9e7c8ccef5d9c3aa2800758c24e7f5

  (defun plantuml-server-encode-url (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((coding-system (or buffer-file-coding-system
                              "utf8"))
           (str (encode-coding-string string coding-system))
           (encoded-string (mapconcat (lambda(x)(format "%02X" x)) str ) ))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string))))
