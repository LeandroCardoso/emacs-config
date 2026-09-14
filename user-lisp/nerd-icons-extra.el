;;; nerd-icons-extra.el --- Extra nerd-icons functionality for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'nerd-icons)

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

;;;###autoload
(defun nerd-icons-icon-for-os-release-id (id)
  "Return the Nerd Font icon corresponding to OS-RELEASE ID."
  (let ((icon (nerd-icons-match-to-alist id nerd-icons-os-release-icon-alist)))
    (when icon
      (apply (car icon) (cdr icon)))))

(nerd-icons-cache #'nerd-icons-icon-for-os-release-id)

(provide 'nerd-icons-extra)

;;; nerd-icons-extra.el ends here
