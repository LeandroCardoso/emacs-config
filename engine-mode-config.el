(defengine cppreference
  "https://en.cppreference.com/mwiki/index.php?search=%s"
  :keybinding "c")

(defengine duckduckgo
  "https://duckduckgo.com/%s"
  :keybinding "d")

(defengine emacswiki
  "https://duckduckgo.com/?q=site:emacswiki.org+%s"
  :keybinding "e")

(defengine google
  "https://www.google.com/search?q=%s"
  :keybinding "g")

(defengine git
  "https://git-scm.com/search/results?search=%s&language=en"
  :keybinding "G")

(defengine microsoft
  "https://learn.microsoft.com/en-us/search/?category=Documentation&terms=%s"
  :keybinding "m")

(defengine csharp
  "https://docs.microsoft.com/en-us/dotnet/api/?term=%s"
  :keybinding "s")

(defengine urbandictionary
  "https://www.urbandictionary.com/define.php?term=%s"
  :keybinding "u")

(defengine wikipedia
  "https://en.wikipedia.org/w/index.php?search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://en.wiktionary.org/w/index.php?search=%s"
  :keybinding "y")

;; RDI
(when rdi-p
  (defengine jira-mcd
    "https://mcd-tools.atlassian.net/jira/search?searchString=%s"
    :keybinding "j")

  (defengine jira-rdi
    "https://jira.rdisoftware.com/jira/secure/QuickSearch.jspa?searchString=%s"
    :keybinding "J"))
