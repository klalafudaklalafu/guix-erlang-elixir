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

(define-module (blitzkrieg build mix-build-system)
  #:use-module ((guix build rebar-build-system) #:prefix rebar:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            mix-build))

(define %elixir-libdir "/lib/elixir/lib")
(define %mix-env "default")

(define* (build #:key (mix-env %mix-env) (mix-flags '()) #:allow-other-keys)
  (setenv "MIX_ENV" mix-env)
  (apply invoke "mix" "compile" "--no-deps-check" mix-flags))

(define* (check #:key (mix-env %mix-env) (mix-flags '()) target (tests? (not target))
                #:allow-other-keys)
  (if tests?
      (begin
        (setenv "MIX_ENV" mix-env)
        (apply invoke "mix" "test" mix-flags))
      (format #t "test suite not run~%")))

(define (elixir-package? name)
  "Check if NAME correspond to the name of an Elixir package."
  (string-prefix? "elixir-" name))

(define (package-name-version->elixir-name name+ver)
  "Convert the Guix package NAME-VER to the corresponding Elixir name-version
format.  Essentially drop the prefix used in Guix and replace dashes by
underscores."
  (let ((name- (package-name->name+version name+ver)))
    (string-join
     (string-split
      (if (elixir-package? name-)       ; checks for "elixir-" prefix
          (string-drop name- (string-length "elixir-"))
          name-)
      #\-)
     "_")))

(define (list-directories directory)
  "Return file names of the sub-directory of DIRECTORY."
  (scandir directory
           (lambda (file)
             (and (not (member file '("." "..")))
                  (file-is-directory? (string-append directory "/" file))))))

(define* (install #:key name outputs
                  (mix-env %mix-env)
                  (install-name (package-name-version->elixir-name name))
                  (install-profile "default") ; build profile outputs to install
                  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (pkg-dir (string-append out %elixir-libdir "/" install-name)))
    (let ((bin-dir (string-append "_build/" install-profile "/bin"))
          (lib-dir (string-append "_build/" install-profile "/lib")))
      ;; install _build/PROFILE/bin
      (when (file-exists? bin-dir)
        (copy-recursively bin-dir out #:follow-symlinks? #t))
      ;; install _build/PROFILE/lib/*/{ebin,include,priv}
      (for-each
       (lambda (*)
         (for-each
          (lambda (dirname)
            (let ((src-dir (string-append lib-dir "/" * "/" dirname))
                  (dst-dir (string-append pkg-dir "/" dirname)))
              (when (file-exists? src-dir)
                (copy-recursively src-dir dst-dir #:follow-symlinks? #t))
              (false-if-exception
               (delete-file (string-append dst-dir "/.gitignore")))))
          '("ebin" "include" "priv")))
       (list-directories lib-dir))
      (false-if-exception
       (delete-file (string-append pkg-dir "/priv/Run-eunit-loop.expect"))))))

(define %standard-phases
  (modify-phases rebar:%standard-phases
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (mix-build #:key inputs (phases %standard-phases)
                    #:allow-other-keys
                    #:rest args)
  "Build the given Elixir package, applying all of PHASES on order."
  (apply rebar:rebar-build #:inputs inputs #:phases phases args))
