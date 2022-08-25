;; engine
(when (require 'engine-mode nil t)
  (defengine jira-rdi
    "https://jira.rdisoftware.com/jira/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "j")
  (defengine jira-mcd
    "https://us-jira.mcd.com/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "J"))
;; Local Variables:
;; no-native-compile: t
;; End:
