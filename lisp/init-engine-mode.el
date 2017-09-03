(when (require 'engine-mode nil t)
  (engine-mode t)

  (defengine cppreference
    "http://en.cppreference.com/mwiki/index.php?search=%s"
    :keybinding "c")

  (defengine google
    "https://www.google.com/search?q=%s"
    :keybinding "g")

  (defengine msdn
    "https://social.msdn.microsoft.com/search/en-US/windows?query=%s"
    :keybinding "m")
  )
