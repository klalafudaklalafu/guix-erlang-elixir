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
      (version "26.1.2")
      (source (origin
                (method url-fetch)
                (uri (string-append uri-prefix version "/otp_src_" version ".tar.gz"))
                (sha256
                 (base32
                  "089r67h0q3gkqn5sags58c16m9rgmgmspm97c3k8f7sglprlq1zi"))
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
               "0g1qv8bx25f92qngz60xyakgj4ny18z5b62x4wcfs83k7gyk3mgn")))))))))
