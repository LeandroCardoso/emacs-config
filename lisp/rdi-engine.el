;; engine
(when (require 'engine-mode nil t)
  (defengine jira-rdi
    "https://jira.rdisoftware.com/jira/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "J")
  (defengine jira-mcd
    "https://mcd-tools.atlassian.net/jira/search?searchString=%s"
    :keybinding "j"))

;; Local Variables
;; no-native-compile: t
;; End:
