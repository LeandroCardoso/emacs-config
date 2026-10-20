;;; system-information.el --- Extra System Information commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'frame)
(require 'custom)
(require 'seq)
(require 'time)

(require 'nerd-icons)

;;; User variables

(defcustom display-system-misc-info nil
  "Additional information displayed by `display-system-info'.

Each element is a list of one of the following forms:

  (LABEL VALUE)
  (LABEL VALUE SHORT-LABEL)
  (LABEL VALUE SHORT-LABEL SHORT-VALUE)

LABEL and VALUE are used in detailed mode.

SHORT-LABEL and SHORT-VALUE customize the display in brief mode.  When
SHORT-LABEL is omitted, LABEL is used.  When SHORT-VALUE is omitted,
VALUE is used.

If VALUE (or SHORT-VALUE) is a string, it is displayed verbatim.
Otherwise, it is treated as a boolean value and displayed as \"yes\"
when non-nil and \"no\" when nil."
  :type '(repeat
          (list
           (string :tag "Label")
           (sexp :tag "Value")
           (choice
            (const :tag "No short label" nil)
            (string :tag "Short label"))
           (choice
            (const :tag "No short label" nil)
            (sexp :tag "Short value"))))
  :group 'display)

;;; nerd-icons integration

(defvar nerd-icons-os-release-icon-alist
  '(("almalinux"              nerd-icons-flicon "nf-linux-almalinux")
    ("alpine"                 nerd-icons-flicon "nf-linux-alpine")
    ("aosc"                   nerd-icons-flicon "nf-linux-aosc")
    ("arch"                   nerd-icons-flicon "nf-linux-archlinux")
    ("archcraft"              nerd-icons-flicon "nf-linux-archcraft")
    ("archlabs"               nerd-icons-flicon "nf-linux-archlabs")
    ("arcolinux"              nerd-icons-flicon "nf-linux-arcolinux")
    ("artix"                  nerd-icons-flicon "nf-linux-artix")
    ("biglinux"               nerd-icons-flicon "nf-linux-biglinux")
    ("cachyos"                nerd-icons-flicon "nf-linux-cachyos")
    ("centos"                 nerd-icons-flicon "nf-linux-centos")
    ("coreos"                 nerd-icons-flicon "nf-linux-coreos")
    ("crystal"                nerd-icons-flicon "nf-linux-crystal")
    ("debian"                 nerd-icons-flicon "nf-linux-debian")
    ("deepin"                 nerd-icons-flicon "nf-linux-deepin")
    ("devuan"                 nerd-icons-flicon "nf-linux-devuan")
    ("elementary"             nerd-icons-flicon "nf-linux-elementary")
    ("endeavouros"            nerd-icons-flicon "nf-linux-endeavour")
    ("fedora"                 nerd-icons-flicon "nf-linux-fedora")
    ("freebsd"                nerd-icons-flicon "nf-linux-freebsd")
    ("garuda"                 nerd-icons-flicon "nf-linux-garuda")
    ("gentoo"                 nerd-icons-flicon "nf-linux-gentoo")
    ("guix"                   nerd-icons-flicon "nf-linux-gnu_guix")
    ("hyperbola"              nerd-icons-flicon "nf-linux-hyperbola")
    ("illumos"                nerd-icons-flicon "nf-linux-illumos")
    ("kali"                   nerd-icons-flicon "nf-linux-kali_linux")
    ("linuxmint"              nerd-icons-flicon "nf-linux-linuxmint")
    ("locos"                  nerd-icons-flicon "nf-linux-locos")
    ("lxle"                   nerd-icons-flicon "nf-linux-lxle")
    ("mageia"                 nerd-icons-flicon "nf-linux-mageia")
    ("mandriva"               nerd-icons-flicon "nf-linux-mandriva")
    ("manjaro"                nerd-icons-flicon "nf-linux-manjaro")
    ("mxlinux"                nerd-icons-flicon "nf-linux-mxlinux")
    ("neon"                   nerd-icons-flicon "nf-linux-kde_neon")
    ("nixos"                  nerd-icons-flicon "nf-linux-nixos")
    ("nobara"                nerd-icons-flicon "nf-linux-nobara")
    ("opensuse"              nerd-icons-flicon "nf-linux-opensuse")
    ("opensuse-leap"         nerd-icons-flicon "nf-linux-opensuse")
    ("opensuse-tumbleweed"   nerd-icons-flicon "nf-linux-opensuse")
    ("parabola"              nerd-icons-flicon "nf-linux-parabola")
    ("parrot"                nerd-icons-flicon "nf-linux-parrot")
    ("pop"                   nerd-icons-flicon "nf-linux-pop_os")
    ("postmarketos"          nerd-icons-flicon "nf-linux-postmarketos")
    ("puppy"                 nerd-icons-flicon "nf-linux-puppy")
    ("qubes"                 nerd-icons-flicon "nf-linux-qubesos")
    ("raspbian"              nerd-icons-flicon "nf-linux-raspberry_pi")
    ("rhel"                  nerd-icons-flicon "nf-linux-redhat")
    ("rocky"                 nerd-icons-flicon "nf-linux-rocky_linux")
    ("sabayon"               nerd-icons-flicon "nf-linux-sabayon")
    ("sles"                  nerd-icons-flicon "nf-linux-opensuse")
    ("sles_sap"              nerd-icons-flicon "nf-linux-opensuse")
    ("sled"                  nerd-icons-flicon "nf-linux-opensuse")
    ("slackware"              nerd-icons-flicon "nf-linux-slackware")
    ("solus"                 nerd-icons-flicon "nf-linux-solus")
    ("tails"                 nerd-icons-flicon "nf-linux-tails")
    ("trisquel"              nerd-icons-flicon "nf-linux-trisquel")
    ("ubuntu"                nerd-icons-flicon "nf-linux-ubuntu")
    ("void"                  nerd-icons-flicon "nf-linux-void")
    ("xerolinux"             nerd-icons-flicon "nf-linux-xerolinux")
    ("zorin"                 nerd-icons-flicon "nf-linux-zorin"))
  "Mapping of `os-release' IDs to Nerd Font glyphs.")

(defun nerd-icons-icon-for-os-release-id (id)
  "Return the Nerd Font icon corresponding to OS-RELEASE ID."
  (let ((icon (nerd-icons-match-to-alist id nerd-icons-os-release-icon-alist)))
    (when icon
      (apply (car icon) (cdr icon)))))

(nerd-icons-cache #'nerd-icons-icon-for-os-release-id)

;;; Internal functions

(defun os-release-info (parameter)
  "Return the operating system release (os-release) PARAMETER value."
  (when (file-readable-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (when (re-search-forward (format "^%s=\\(.+\\)$" parameter) nil t)
        (string-trim (match-string-no-properties 1) "\"" "\"")))))

(defun message-summary-data (detailed &rest fields)
  "Display a summary of FIELDS in the echo area.

Each element of FIELDS is either a field or a list of fields.

A field has one of the following forms:

  (LABEL VALUE)
  (LABEL VALUE SHORT-LABEL)
  (LABEL VALUE SHORT-LABEL SHORT-VALUE)

LABEL and VALUE are used in detailed mode.

SHORT-LABEL and SHORT-VALUE customize the display in brief mode.  When
SHORT-LABEL is omitted, LABEL is used.  When SHORT-VALUE is omitted,
VALUE is used.

If VALUE (or SHORT-VALUE) is a string, it is displayed verbatim.
Otherwise, it is treated as a boolean value and displayed as \"yes\"
when non-nil and \"no\" when nil.

When DETAILED is non-nil, labels are shown and fields are separated by
newlines."
  (let* ((separator (if detailed "\n" " | "))
         ;; flatten the internal list struct, necessary to use the `display-system-misc-info'
         (fields (seq-mapcat (lambda (field)
                               (if (and (listp field)
                                        (listp (car field)))
                                   field
                                 (list field)))
                             fields))
         (label-width (when detailed
                        (apply #'max (mapcar (lambda (field) (string-width (car field))) fields)))))
    (message "%s"
             (mapconcat (pcase-lambda (`(,label ,value ,short-label, short-value))
                          (format (cond
                                   (detailed (format "%%-%ds %%s" label-width))
                                   ;; In short mode, when label is an empty string, don't put a
                                   ;; space between the label and the value, so it can be display
                                   ;; like in the example: "value1 | value2"
                                   ((string-empty-p (or (and (not detailed) short-label) label)) "%s%s")
                                   (t "%s %s"))
                                  (or (and (not detailed) short-label) label)
                                  (cond
                                   ((and (not detailed) (stringp short-value)) short-value)
                                   ((stringp value) value)
                                   ((or short-value value) "yes")
                                   (t "no"))))
                        fields
                        separator))))

(defun system-memory-usage ()
  "Return a list of (USED-RAM TOTAL-RAM).

All values are in Kbytes.  If the system is not supported or memory
information can’t be obtained, return nil."
  (let ((meminfo-file "/proc/meminfo"))
    (when (file-readable-p meminfo-file)
      (with-temp-buffer
        (insert-file-contents meminfo-file)
        (let ((total (progn (goto-char (point-min))
                            (re-search-forward "^MemTotal:[ \t]+\\([0-9]+\\) kB")
                            (string-to-number (match-string 1))))
              (available (progn (goto-char (point-min))
                                (re-search-forward "^MemAvailable:[ \t]+\\([0-9]+\\) kB")
                                (string-to-number (match-string 1)))))
          (list (- total available) total))))))

;;; Commands

;;;###autoload
(defun display-font-information (&optional detailed)
  "Display current default font information.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (message-summary-data detailed
                        `("Name:"
                          ,(face-attribute 'default :family)
                          ,(nerd-icons-faicon "nf-fa-font"))
                        `("Weight:"
                          ,(format "%s" (face-attribute 'default :weight))
                          "")
                        `("Width:"
                          ,(format "%s" (face-attribute 'default :width))
                          "")
                        `("Size:"
                          ,(format "%s" (round (/ (face-attribute 'default :height) 10.0)))
                          "")))

;;;###autoload
(defun display-monitor-layout-info (&optional detailed)
  "Display current monitor, frame and selected window dimensions.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let ((monitor-width (nth 2 (frame-monitor-attribute 'geometry)))
        (monitor-height (nth 3 (frame-monitor-attribute 'geometry))))
    (message-summary-data detailed
                          `("Monitor:" ,(format "%dx%d" monitor-width monitor-height))
                          `("Frame:" ,(format "%dx%d" (frame-width) (frame-height)))
                          `("Window:" ,(format "%dx%d" (window-body-width) (window-body-height))))))

;;;###autoload
(defun display-system-info (&optional detailed)
  "Display system information.

With prefix argument DETAILED, display a detailed message, instead of a
brief one."
  (interactive "P")
  (let* ((memory-info (when-let* ((memory (system-memory-usage)))
                        (mapcar #'(lambda (arg) (/ (float arg) (expt 1024 2))) memory)))
         (system-icon (or (nerd-icons-icon-for-os-release-id (os-release-info "ID"))
                          (pcase system-type
                            ('gnu/linux (nerd-icons-flicon "nf-linux-tux"))
                            ('windows-nt (nerd-icons-devicon "nf-dev-windows"))
                            ('darwin (nerd-icons-devicon "nf-dev-apple"))))))
    (message-summary-data detailed
                          `("Emacs version:"
                            ,emacs-version
                            ,(nerd-icons-sucicon "nf-custom-emacs"))
                          `("System:"
                            ,(format "%s (%s)" (or (os-release-info "PRETTY_NAME") system-type) window-system)
                            ,system-icon
                            ,(format "%s (%s)" (or (os-release-info "NAME") system-type) window-system))
                          `("User:"
                            ,(format "%s | %s" user-login-name user-mail-address)
                            ,(nerd-icons-faicon "nf-fa-user")
                            ,user-login-name)
                          `("Hostname:" ,(system-name) ,(nerd-icons-faicon "nf-fa-desktop"))
                          display-system-misc-info
                          `("Uptime:"
                            ,(emacs-uptime)
                            ,(nerd-icons-faicon "nf-fa-clock")
                            ,(emacs-uptime "%D, %z%2h:%.2m"))
                          `("Load average:"
                            ,(apply #'format "%.2f %.2f %.2f" (load-average t))
                            ,(nerd-icons-faicon "nf-fa-microchip"))
                          `("Memory:"
                            ,(if memory-info (format "%.1f/%.1f GiB" (car memory-info) (cadr memory-info)) "N/A")
                            ,(nerd-icons-faicon "nf-fa-memory")
                            ,(if memory-info (format "%.1f GiB" (car memory-info)) "N/A"))
                          `("Started in:"
                            ,(emacs-init-time "%.2f seconds")
                            ,(nerd-icons-faicon "nf-fa-rocket")
                            ,(emacs-init-time "%.2fs")))))

(provide 'system-information)

;;; system-information.el ends here
