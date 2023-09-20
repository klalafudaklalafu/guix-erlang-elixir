(define-module (blitzkrieg packages playground)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages certs)
  #:use-module (guix git-download)
  #:use-module (blitzkrieg build-system mix))

(define-public playground
  (package
    (name "playground")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.blitzkrieg.dev/playground/")
                    (commit "bf8f1e4316b26d1896012010a99f6ba1d1b143dd")))
              (sha256
               (base32
                "0ilkks0w6p90fc94qrj1jq5b3cinckihqyv0gk1ihcy9i83d342h"))))
    (build-system mix-build-system)
    (arguments
     '(#:mix-env "prod"))
    ;; '(#:mix-flags '()))
    ;; (inputs (list "elixir-hex"))
    (synopsis "some brief description")
    (description
     "some full description")
    (home-page "https://blitzkrieg.dev")
    (license agpl3+)))
