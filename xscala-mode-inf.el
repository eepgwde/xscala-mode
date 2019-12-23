;;; -*-Emacs-Lisp-*-
;;; xscala-mode-inf.el - Interaction with a Scala interpreter.

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

(provide 'xscala-mode-inf)


(require 'comint)

(defgroup xscala-mode-inf nil
  "Mode to interact with a Scala interpreter."
  :group 'xscala
  :tag "Inferior Scala")

(defvar xscala-interpreter "sbt"
  "The interpreter that `run-xscala' should run. This should
 be a program in your PATH or the full pathname of the xscala interpreter.")

(defvar xscala-args "console"
  "The arguments for the `run-xscala' should run.")

(defcustom xscala-std-interpreter "scala"
  "The interpreter that `run-xscala' should run. This should
 be a program in your PATH or the full pathname of the xscala interpreter."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-spark-interpreter "spark-shell"
  "The interpreter that `run-xscala' should run to use a Spark cluster. This should
 be a program in your PATH or the full pathname of the Spark interpreter."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-edit-mark "// #mark"
  "String to insert."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-edit-mark-re nil
  "Regular expression for a line to mark the end of a block to send to the interpreter. Derived from `xscala-edit-mark' by prefixing with ^."
  :type 'string
  :group 'xscala-mode-inf)

(defcustom xscala-std-options nil
  "*List of Scala interpreter options."
  :type '(repeat string)
  :group 'xscala-mode-inf)

(defcustom xscala-spark-options nil
  "*List of Spark Scala interpreter options."
  :type '(repeat string)
  :group 'xscala-mode-inf)

(defconst xscala-inf-buffer-name0 "*compilation*")

(defconst xscala-inf-buffer-name "*compilation*")

(define-derived-mode xscala-mode-inf comint-mode "Inferior Scala"
  "Major mode for interacting with a Scala interpreter.

\\{inferior-xscala-mode-map\\}"
  (define-key xscala-mode-inf-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'xscala-input-sender))

(defun xscala-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

;; (with-current-buffer buffer
;;  (xscala-mode-inf))

;;;###autoload
(defun xscala-interpreter-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc xscala-inf-buffer-name)
  (with-current-buffer xscala-inf-buffer-name
    (xscala-mode-inf)
    (read-only-mode -1))
  (comint-check-proc xscala-inf-buffer-name))

(defun xscala-check-interpreter-running ()
  (unless (xscala-interpreter-running-p-1)
    (error "Scala interpreter not running")))

;;;###autoload
(defun xscala-run-xscala (cmd-line)
  "Run a Scala interpreter in an Emacs buffer"
  (interactive (list (if current-prefix-arg
			 (read-string "Scala interpreter: " xscala-interpreter)
                       xscala-interpreter)))
  (unless (xscala-interpreter-running-p-1)
    (setq xscala-inf-buffer-name xscala-inf-buffer-name0)
    (setq xscala-interpreter cmd-line)
    (setq cmd-line (format "%s %s" xscala-interpreter xscala-args))
    (let ((cmd/args (split-string cmd-line)))
      (set-buffer
       (apply 'make-comint "inferior-xscala" (car cmd/args) nil (cdr cmd/args))))
    (xscala-mode-inf)
    (pop-to-buffer xscala-inf-buffer-name)))

(defun xscala-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string xscala-inf-buffer-name (apply 'format str args))
  ;; (comint-send-string xscala-inf-buffer-name "\nemacs:end\n")) Heineman's contrib (06/03/2007)
  (comint-send-string xscala-inf-buffer-name "\n"))

;;;###autoload
(defun xscala-switch-to-interpreter ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (xscala-check-interpreter-running)
  (switch-to-buffer-other-window xscala-inf-buffer-name)
  (end-of-buffer))

(defvar xscala-tmp-file nil)

;;;###autoload
(defun xscala-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (xscala-check-interpreter-running)
  (comint-send-region xscala-inf-buffer-name start end)
  (comint-send-string xscala-inf-buffer-name "\n"))

;;;###autoload
(defun xscala-eval-definition ()
  "Send the current 'definition' to the Scala interpreter.
This function's idea of a definition is the block of text ending
in the current line (or the first non-empty line going
backwards), and begins in the first line that is not empty and
does not start with whitespace or '{'.

For example:

println( \"aja\")
println( \"hola\" )

if the cursor is somewhere in the second print statement, the
interpreter should output 'hola'.

In the following case, if the cursor is in the second line, then
the complete function definition will be send to the interpreter:

def foo =
  1 + 2
"
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat #'(lambda (x) x)
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that end with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (xscala-eval-region (point) end))))

;;; Send a paragraph and step forward.
;;; This doesn't work at all well
;;;###autoload
(defun xscala-eval-step ()
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (forward-paragraph)
      (xscala-eval-region beg (point)) ))
  (forward-paragraph))

;;; Flips between the standard and the Spark interpreter.
(defun xscala-toggle ()
  (interactive)
  (setq xscala-interpreter (if (string= xscala-interpreter xscala-std-interpreter) 
			      (symbol-value 'xscala-spark-interpreter)
			    (symbol-value 'xscala-std-interpreter)))

  (setq xscala-args (if (string= xscala-interpreter xscala-std-interpreter) 
		       (mapconcat 'identity (default-value 'xscala-std-options) " ")
		     (mapconcat 'identity (default-value 'xscala-spark-options) " ")) )
    
  (message "xscala-interpreter: \"%s\"" xscala-interpreter) )

;;;###autoload
(defun xscala-eval-paste-region (start end)
  "Send current region to Scala interpreter as a paste."
  (interactive "r")
  (xscala-check-interpreter-running)
  (comint-send-string xscala-inf-buffer-name ":paste\n")
  (comint-send-region xscala-inf-buffer-name start end)
  (comint-send-string xscala-inf-buffer-name "\n")
  (let ((src0 (current-buffer)))
    (switch-to-buffer xscala-inf-buffer-name)
    (comint-send-string xscala-inf-buffer-name "\n")
    (comint-send-eof)
    (comint-send-eof)
    (pop-to-buffer src0))
  )

(defun xscala-eval-paste-mark ()
  (interactive "r")
  (save-excursion 
  (let ((beg (point)))
    (re-search-forward xscala-edit-mark-re)
    (beginning-of-line)
    (xscala-eval-paste-region beg (point)) )) )

(defun xscala-eval-mark ()
  (interactive "r")
  (save-excursion 
  (let ((beg (point)))
    (re-search-forward xscala-edit-mark-re)
    (beginning-of-line)
    (xscala-eval-region beg (point)) )) )

(defun xscala-eval-paste-mark-step ()
  (interactive)
  (xscala-eval-paste-mark)
  (re-search-forward xscala-edit-mark-re)
  (next-line) )

(defun xscala-eval-mark-step ()
  (interactive)
  (xscala-eval-mark)
  (re-search-forward xscala-edit-mark-re)
  (next-line) )

(defun xscala-mark-backward ()
  (interactive)
  (next-line -1)
  (re-search-backward xscala-edit-mark-re)
  (next-line))

(defun xscala-mark-forward ()
  (interactive)
  (next-line)
  (re-search-forward xscala-edit-mark-re))

(defun xscala-mark-insert ()
  (interactive)
  (next-line)
  (re-search-forward xscala-edit-mark-re))

;;; Very useful. 
;;;  - mark where I am
;;;  - go back a mark
;;;  - execute to next mark
;;;  - go back to where I was
(defun xscala-eval-marks ()
  (interactive)
  (save-excursion
    (xscala-mark-backward)
    (xscala-eval-mark) ) )


;;;###autoload
(defun xscala-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (xscala-eval-region (point-min) (point-max)))

(defvar xscala-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last xscala-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun xscala-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " xscala-prev-l/c-dir/file
				  '(xscala-mode) t))
  (xscala-check-interpreter-running)
  (comint-check-source file-name)
  (setq xscala-prev-l/c-dir/file (cons (file-name-directory file-name)
                                      (file-name-nondirectory file-name)))
  (xscala-send-string ":load %s" file-name))

;;;###autoload
(defun xscala-quit-interpreter ()
  "Quit Scala interpreter."
  (interactive)
  (xscala-check-interpreter-running)
  (xscala-send-string "\n:quit"))

(defun xscala-find-interpreter ()
  "Find Scala interpreter."
  (interactive)
  (xscala-interpreter-running-p-1))

