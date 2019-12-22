;;; -*-Emacs-Lisp-*-
;;; xscala-mode.el - Major mode for editing Scala code.

;; Copyright (C) 2009-2011 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: xscala languages oop

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2011 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'xscala-mode)

(require 'cl)

(require 'xscala-mode-constants)
(require 'xscala-mode-variables)
(require 'xscala-mode-lib)
(require 'xscala-mode-navigation)
(require 'xscala-mode-ui)
(require 'xscala-mode-feature)

;;; Customization and Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xscala-mode-hook nil
  "Hook to run after installing xscala mode")

(defgroup xscala nil
  "Mode for editing Scala code."
  :group 'languages)

(defcustom xscala-mode:api-url "http://www.xscala-lang.org/docu/files/api/index.html"
  "URL to the online Scala documentation"
  :type 'string
  :group 'xscala)

(defconst xscala-mode-version "0.5.99.5")
(defconst xscala-mode-svn-revision "$Revision: 21917 $")
(defconst xscala-bug-e-mail "xscala@listes.epfl.ch")
(defconst xscala-web-url "http://xscala-lang.org/")


;;; Helper functions/macroes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xscala-mode:browse-web-site ()
  "Browse the Scala home-page"
  (interactive)
  (require 'browse-url)
  (browse-url xscala-web-url))


(defun xscala-mode:browse-api ()
  "Browse the Scala API"
  (interactive)
  (require 'browse-url)
  (browse-url xscala-mode:api-url))

(defvar xscala-mode-abbrev-table nil
  "Abbrev table in use in `xscala-mode' buffers.")
(define-abbrev-table 'xscala-mode-abbrev-table nil)


;;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode xscala-minor-mode
  "Toggle Xscala minor mode.
See the command `xscala-mode' for more information on this mode."
  nil " Xscala" (list (cons [menu-bar] xscala-mode-menu-bar-map))
  :group 'xscala
  (if xscala-minor-mode
      (progn
	      ;; Turn off this mode if we change major modes.
	      (add-hook 'change-major-mode-hook
		              (lambda () (xscala-minor-mode -1))
		              nil t)

        ;; load the keys
        (use-local-map xscala-mode-map)

        ;; These will be local to the file and we take the defaults.
        (make-local-variable 'xscala-std-options)
        (make-local-variable 'xscala-spark-options)
        ;; make a copy
        (make-local-variable 'xscala-interpreter)
        (setq xscala-interpreter xscala-std-interpreter)

        ;; These might be specific to the style in the file.
        (make-local-variable 'xscala-edit-mark)
        (make-local-variable 'xscala-edit-mark-re)
 
        (if (not xscala-edit-mark-re) 
            (setq xscala-edit-mark-re (concat "^" outline-regexp)) )

        )
    ;; Cause use of ellipses for invisible text.
    ;; When turning off xscala mode, get rid of any xscala hiding.
    ;; (xscala-show-all))
    )
  )

