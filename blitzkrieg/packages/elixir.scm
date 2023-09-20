;;; SPDX-License-Identifier: GPL-3.0-or-later
;; Elixir package.

(define-module (blitzkrieg packages elixir)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module ((gnu packages elixir) #:prefix gnu:)
  #:use-module (blitzkrieg build-system mix)
  #:use-module (blitzkrieg packages erlang))

(define-public elixir
  (let* ((elixir-erl-compiler-path "lib/elixir/src/elixir_erl_compiler.erl")
         (elixir-erl-compiler-path-orig (string-append elixir-erl-compiler-path ".orig")))
    (package/inherit gnu:elixir
      (name "elixir")
      (version "1.15.5")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/elixir-lang/elixir")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1b9mxlb0x301lmjd5wvhw23vliv265wx384bpd76phk0grx73kfq"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gnu:elixir)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'replace-paths 'pre-install-source
                (lambda _
                  (copy-recursively "lib" (string-append #$output "/source/lib"))))
              (add-after 'pre-install-source 'patch-elixir-compiler
                (lambda _
                  (copy-recursively #$elixir-erl-compiler-path #$elixir-erl-compiler-path-orig)
                  (let ((source (string-append "/tmp/guix-build-elixir-" #$version ".drv-0"))
                        (destination #$output))
                    (substitute* #$elixir-erl-compiler-path
                      (("source, Source")
                       (string-append "source, string:replace(Source, \"" source "\", \"" destination "\")"))))))
              (add-after 'build 'restore-and-recompile
                (lambda _
                  (copy-recursively #$elixir-erl-compiler-path-orig #$elixir-erl-compiler-path)
                  (invoke "make")))))))
      (inputs
       (list erlang git)))))

;; stolen from https://git.sr.ht/~sokolov/guix/
(define-public elixir-hex
  (package
    (name "elixir-hex")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hexpm/hex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvczwvij58kgkhak68004ap81pl26600bczg21mymy2sypkgxmj"))))
    (build-system mix-build-system)
    (arguments
     ;; Avoiding circular dependency
     (list #:hex #f
           #:tests? #f))
    (home-page "https://hex.pm")
    (synopsis "Package manager for the Erlang VM")
    (description #f)
    (license license:asl2.0)))
