;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ivan Sokolov <ivan-p-sokolov@ya.ru>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (blitzkrieg build-system mix)
  #:use-module ((guix monads)   #:select (mlet))
  #:use-module ((guix packages) #:select (default-guile package->derivation))
  #:use-module ((guix store)    #:select (%store-monad))
  #:use-module ((guix utils)    #:select (%current-system strip-keyword-arguments))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:export (%mix-build-system-modules
            mix-build
            mix-build-system))

(define %mix-build-system-modules
  ;; Build-side modules imported by default.
  `((blitzkrieg build mix-build-system)
    (guix build rebar-build-system)
    ,@%gnu-build-system-modules))

(define (default-elixir)
  "Return the default Elixir package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((elixir-mod (resolve-interface '(blitzkrieg packages elixir))))
    (module-ref elixir-mod 'elixir)))

(define (default-hex)
  "Return the default Hex package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((elixir-mod (resolve-interface '(blitzkrieg packages elixir))))
    (module-ref elixir-mod 'elixir-hex)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (elixir (default-elixir))
                (hex (default-hex))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    '(#:inputs #:native-inputs #:target #:elixir #:hex))

  (and (not target)                     ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs))
         (build-inputs `(("elixir" ,elixir)
                         ,@(if hex
                               `(("hex" ,hex))
                               '())
                         ,@native-inputs
                         ;; Keep the standard inputs of 'gnu-build-system'.
                         ,@(standard-packages)))
         (outputs outputs)
         (build mix-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define mix-build-system
  (build-system
    (name 'mix)
    (description "The standard Mix build system")
    (lower lower)))

(define* (mix-build name inputs
                    #:key
                    guile source
                    (mix-env "default")
                    (mix-flags ''())
                    (tests? #t)
                    (test-target "test")
                    ;; TODO: install-name  ; default: based on guix package name
                    (install-profile "default")
                    (phases '(@ (blitzkrieg build mix-build-system)
                                %standard-phases))
                    (outputs '("out"))
                    (search-paths '())
                    (native-search-paths '())
                    (system (%current-system))
                    (imported-modules %mix-build-system-modules)
                    (modules '((blitzkrieg build mix-build-system)
                               (guix build utils))))
  "Build SOURCE with INPUTS."

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(mix-build #:source #+source
                           #:system #$system
                           #:name #$name
                           #:mix-env #$mix-env
                           #:mix-flags #$mix-flags
                           #:tests? #$tests?
                           #:test-target #$test-target
                           ;; TODO: #:install-name #$install-name
                           #:install-profile #$install-profile
                           #:phases #$(if (pair? phases)
                                          (sexp->gexp phases)
                                          phases)
                           #:outputs %outputs
                           #:search-paths '#$(sexp->gexp
                                              (map search-path-specification->sexp
                                                   search-paths))
                           #:inputs %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    ;; Note: Always pass #:graft? #f.  Without it, ALLOWED-REFERENCES &
    ;; co. would be interpreted as referring to grafted packages.
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:guile-for-build guile)))
