;;; SPDX-License-Identifier: GPL-3.0-or-later
;; Erlang package.

(define-module (blitzkrieg packages erlang)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module ((gnu packages erlang) #:prefix gnu:)
  #:use-module (gnu packages perl))

(define-public erlang
  (let ((uri-prefix "https://github.com/erlang/otp/releases/download/OTP-"))
    (package/inherit gnu:erlang
      (name "erlang")
      (version "26.1.1")
      (source (origin
                (method url-fetch)
                (uri (string-append uri-prefix version "/otp_src_" version ".tar.gz"))
                (sha256
                 (base32
                  "0mxgc39zfah9cl56wwbfcmczja2dm745nbifxxrmrwffhzk5dpih"))
                (patches (search-patches "erlang-man-path.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gnu:erlang)
         ((#:configure-flags flags #~'())
          #~(cons* "--enable-dirty-schedulers"
                   "--enable-kernel-poll"
                   "--enable-esock"
                   "--without-javac"
                   #$flags))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install-doc 'install-chunks
                (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "make" "install-docs" "DOC_TARGETS=chunks")))))))
      (native-inputs
       `(("perl" ,perl)
         ("erlang-manpages"
          ,(origin
             (method url-fetch)
             (uri (string-append uri-prefix version "/otp_doc_man_" version ".tar.gz"))
             (sha256
              (base32
               "1drdg6dzm58i228zn6r5v923b3yh1ygynypfk5j1jdq04sxqqbbr")))))))))
