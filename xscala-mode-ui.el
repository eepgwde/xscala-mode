;;; -*-Emacs-Lisp-*-
;;; xscala-mode-ui.el - Menu entries and keyboard shortcuts for xscala mode

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

(provide 'xscala-mode-ui)

(require 'easymenu)
(require 'xscala-mode-lib)

(eval-when-compile
  (require 'xscala-mode-inf))

(eval-and-compile
  (defcustom xscala-mode-ui:prefix-key "\C-c\""
    "Key prefix for xscala mode."
    :group 'xscala))

(defmacro xscala-mode-ui:key (key)
  "Simple macro for appending 'xscala-mode-prefix-key' to key commands"
  `(kbd ,(concat xscala-mode-ui:prefix-key " " key)))

;;; Helper functions

(defun xscala-mode-ui:interpreter-running-p ()
  "True iff a Scala interpreter is currently running in a buffer."
  ;; The following makes sure that we do not autoload
  ;; xscala-mode-inf just to check if the interpreter is running.
  (and (fboundp 'xscala-mode-inf)
       (let ((ism-def (symbol-function 'xscala-mode-inf)))
         (not (and (consp ism-def) (eq (car ism-def) 'autoload))))
       (xscala-interpreter-running-p-1)))


;;; Shortcuts

(defvar xscala-mode-map
  (let ((map (make-keymap)))
    map))

(xscala-mode-lib:define-keys xscala-mode-map

   ;; Attach Menubar
   ;; ([menu-bar] xscala-mode-menu-bar-map)

   ;; Attach keyboard Shortcuts
   		                
   ((xscala-mode-ui:key "x o")   'xscala-switch-to-interpreter)
   ((xscala-mode-ui:key "x l")   'xscala-load-file)
   ((xscala-mode-ui:key "x r")   'xscala-eval-region)
   ((xscala-mode-ui:key "x d")   'xscala-eval-definition)
   ((xscala-mode-ui:key "x c")   'xscala-eval-step)
   ((xscala-mode-ui:key "x b")   'xscala-eval-buffer)
   ((xscala-mode-ui:key "p r")  'xscala-eval-paste-region)
   ((xscala-mode-ui:key "m b")  'xscala-mark-backward)
   ((xscala-mode-ui:key "m f")  'xscala-mark-forward)
   ((xscala-mode-ui:key "p m")  'xscala-eval-paste-mark-step)
   ((xscala-mode-ui:key "m x")  'xscala-eval-mark-step)
   ([(control c)(control c)]   'xscala-eval-mark-step)
   ([(control c)(control e)]   'xscala-eval-step)
   ([(control c)(control r)]   'xscala-mark-backward)
			        
   ((xscala-mode-ui:key "x j")   'comment-region)
   )


;;; Menubar

(xscala-mode-lib:define-keys xscala-mode-menu-bar-map

  ([xscala]                 (cons "Xscala" (make-sparse-keymap "XScalaMode")))

  ([xscala version]         '(menu-item "Version"                 (lambda () (interactive) (message "Using xscala mode version %s (%s)" xscala-mode-version xscala-mode-svn-revision)) ))
  ([xscala customize]       '(menu-item "Customize"               (lambda () (interactive) (customize-group 'xscala))))
  ([xscala browse-api]      '(menu-item "Browse Scala API"        xscala-mode:browse-api))
  ([xscala browse-website]  '(menu-item "Browse Scala Website"    xscala-mode:browse-web-site))

  ([xscala sep0]            '("---"))

  ([xscala feature]         (cons "Features" (make-sparse-keymap "Features")))

  ([xscala feature paste-r] '(menu-item "Paste region"		        xscala-eval-paste-region :enable (xscala-mode-ui:interpreter-running-p)))

  ([xscala feature paste-m] '(menu-item "Paste to mark"		        xscala-eval-paste-mark-step :enable (xscala-mode-ui:interpreter-running-p)))

  ([xscala feature eval-m]  '(menu-item "Evaluate to mark"	      xscala-eval-mark-step :enable (xscala-mode-ui:interpreter-running-p)))

  ([xscala feature mark-f]  '(menu-item "Forward mark"			      xscala-mark-forward ))
  ([xscala feature mark-b]  '(menu-item "Backward mark"			      xscala-mark-backward ))

  ([xscala feature sep0]     '("---"))

  ([xscala eval-buf]       '(menu-item "Evaluate buffer"          xscala-eval-buffer           :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala eval-reg]       '(menu-item "Evaluate region"          xscala-eval-region           :enable (and (xscala-mode-ui:interpreter-running-p) mark-active)))
  ([xscala eval-def]       '(menu-item "Evaluate definition"      xscala-eval-definition       :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala eval-def]       '(menu-item "Evaluate step"            xscala-eval-step             :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala eval-mark]      '(menu-item "Evaluate to mark"         xscala-eval-mark-step        :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala switch-interp]  '(menu-item "Switch to interpreter"    xscala-switch-to-interpreter :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala load-file]      '(menu-item "Load file in interpreter" xscala-load-file             :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala quit-interp]    '(menu-item "Quit interpreter"         xscala-quit-interpreter      :enable (xscala-mode-ui:interpreter-running-p)                  ))
  ([xscala find-interp]    '(menu-item "Find interpreter"         xscala-find-interpreter      :enable (not (xscala-mode-ui:interpreter-running-p))            ))
  ([xscala run-interp]     '(menu-item "Run interpreter..."       xscala-run-xscala            :enable (not (xscala-mode-ui:interpreter-running-p))            ))
  ([xscala run-sbt]        '(menu-item "Run SBT..."               sbt-start                    :enable (not (xscala-mode-ui:interpreter-running-p))            ))
  ([xscala use-spark]      '(menu-item "Toggle Interpreters..."   xscala-toggle                :enable (not (xscala-mode-ui:interpreter-running-p))            ))
  ([xscala sbt-console]    '(menu-item "SBT Console..."           run-xscala                   :enable (xscala-mode-ui:interpreter-running-p)                  ))

  )


