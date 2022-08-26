(when (require 'ox-jira nil t)
  ;; Override to preserve newlines in paragraphs
  (defun ox-jira-paragraph-override (paragraph contents info)
    "Transcode a PARAGRAPH element from Org to JIRA.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
    (replace-regexp-in-string "\n\\([^\']\\)" " \n\\1" contents))

  (advice-add 'ox-jira-paragraph :override #'ox-jira-paragraph-override))
