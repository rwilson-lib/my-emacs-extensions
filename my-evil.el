;;; my-evil.el --- Intro -*- lexical-binding: t -*-

;; Copyright 2023 Ramus Jabee Lloyd Wilson

;; Author: Ramus Jabee Lloyd Wilson
;; Maintainer: Ramus Jabee Lloyd Wilson
;; Created: 20 May 2023

;; Keywords: evil extension swap
;; Homepage: "https://github.com/rwilson-lib/my-emacs-extensions"
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (evil "1.15.0") (winum "2.2.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom extents and helper function to improve evil mode mapping.
;; Most function advice to allow unviersal arguments just like vim
;;
;; Exported names start with "my-evil-"; private names start with "my-evil--".

;;; Code:

(require 'evil)
(require 'winum)


(evil-define-command evil-sfind (file)
  "Open FILE in a split"
  (interactive "<f>")
  (find-file-other-window file))

(evil-define-command evil-tabedit (file)
  "Open FILE in a new tab"
  (interactive "<f>")
  (find-file-other-tab file))

(evil-define-operator evil-write-delete-buffer (beg end type file-or-append &optional bang)
  "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
   If FILE-OR-APPEND is of the form \" FILE\", append to FILE
   instead of overwriting.  The current buffer's filename is not
   changed unless it has no associated file and no region is
   specified.  If the file already exists and the BANG argument is
   non-nil, it is overwritten without confirmation."

  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><fsh><!>")
  (let* ((append-and-filename (evil-extract-append file-or-append))
	 (append (car append-and-filename))
	 (filename (cdr append-and-filename))
	 (bufname (buffer-file-name (buffer-base-buffer))))
    (when (zerop (length filename))
      (setq filename bufname))
    (cond
     ((zerop (length filename))
      (user-error "Please specify a file name for the buffer"))
     ;; execute command on region
     ((eq (aref filename 0) ?!)
      (shell-command-on-region beg end (substring filename 1)))
     ;; with region or append, always save to file without resetting
     ;; modified flag
     ((or append (and beg end))
      (write-region beg end filename append nil nil (not (or append bang))))
     ;; no current file
     ((null bufname)
      (write-file filename (not bang)))
     ;; save current buffer to its file
     ((string= filename bufname)
      (if (not bang) (save-buffer) (write-file filename)))
     ;; save to another file
     (t
      (write-region nil nil filename
		    nil (not bufname) nil
		    (not bang))))))

(evil-ex-define-cmd "wd[elete]" 'evil-write-delete-buffer)
(evil-ex-define-cmd "sf[ind]" 'evil-sfind)
(evil-ex-define-cmd "tabedit" 'evil-tabedit)

;; example how to map a command in normal mode (called 'normal state' in evil)
(define-key evil-normal-state-map (kbd "z <return>") 'enlarge-window)


(defun custom/force-normal-state-or-exit ()
  ((interactive "P")
  (cond
   ((eq evil-state 'normal) (keyboard-quit))
   (t (evil-force-normal-state))))

(defun tab-bar-switch--advice (orig-fun &rest args)
  "Advice function (ORIG-FUN ARGS) to add universal argument support to `tab-bar switch'."
  (if (null current-prefix-arg)
      (apply orig-fun args)
    (tab-bar-select-tab current-prefix-arg)))

(advice-add 'tab-bar-switch-to-next-tab :around #'tab-bar-switch--advice)
(advice-add 'tab-bar-switch-to-prev-tab :around #'tab-bar-switch--advice)

(defun get-buffer-by-window-index (&optional window index)
  "docstring"
  (interactive "P")
  (if (not (null window))
    (setq index (winum-get-number))
  (setq index (or index current-prefix-arg)))
  (window-buffer (winum-get-window-by-number index)))


(defun evil-delete-buffer--advice (orig-fun &rest args)
  "Advice function (ORIG-FUN ARGS) to add universal argument support to `tab-bar switch'."
  (if (null current-prefix-arg)
      (apply orig-fun args)
    "Advice to modify a specific argument using apply."
    (let ((modified-args (copy-sequence args))) ; Create a copy of the original arguments
      (setf (nth 0 modified-args)
	    (get-buffer-by-window-index
	     nil
	     current-prefix-arg)) ; Modify the third argument (index 2) to 'new-value

      ;; Call the original function with the modified arguments
      (apply orig-fun modified-args))))

(defun evil-window-delete--advice (orig-fun &rest args)
  "Advice function (ORIG-FUN ARGS) to add universal argument support to `tab-bar switch'."
  (if (null current-prefix-arg)
      (apply orig-fun args)
    "Advice to modify a specific argument using apply."
    (let ((modified-args (copy-sequence args))) ; Create a copy of the original arguments
      (setf (nth 0 modified-args)
	    (get-buffer-window
	     (get-buffer-by-window-index
	      nil
	      current-prefix-arg))) ; Modify the third argument (index 2) to 'new-value

      ;; Call the original function with the modified arguments
      (apply orig-fun modified-args))))

(advice-add 'evil-delete-buffer :around #'evil-delete-buffer--advice)
(advice-add 'evil-delete-window :around #'evil-window-delete--advice)

(evil-define-command evil-sfind (file)
  "Open FILE in a split"
  (interactive "<f>")
  (find-file-other-window file))

(evil-define-command evil-tabedit (file)
  "Open FILE in a new tab"
  (interactive "<f>")
  (find-file-other-tab file))

(evil-define-operator evil-write-delete-buffer (beg end type file-or-append &optional bang)
  "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
If FILE-OR-APPEND is of the form \" FILE\", append to FILE
instead of overwriting.  The current buffer's filename is not
changed unless it has no associated file and no region is
specified.  If the file already exists and the BANG argument is
non-nil, it is overwritten without confirmation."
  :motion nil
  :move-point nil
  :type line
      :repeat nil
      (interactive "<R><fsh><!>")
      (let* ((append-and-filename (evil-extract-append file-or-append))
	     (append (car append-and-filename))
	     (filename (cdr append-and-filename))
	     (bufname (buffer-file-name (buffer-base-buffer))))
	(when (zerop (length filename))
	  (setq filename bufname))
	(cond
	 ((zerop (length filename))
	  (user-error "Please specify a file name for the buffer"))
	 ;; execute command on region
	 ((eq (aref filename 0) ?!)
	  (shell-command-on-region beg end (substring filename 1)))
	 ;; with region or append, always save to file without resetting
	 ;; modified flag
	 ((or append (and beg end))
	  (write-region beg end filename append nil nil
			(not (or append bang))))
	 ;; no current file
	 ((null bufname)
	  (write-file filename (not bang)))
	 ;; save current buffer to its file
	 ((string= filename bufname)
	  (if (not bang) (save-buffer) (write-file filename)))
	 ;; save to another file
	 (t
	  (write-region nil nil filename
			nil (not bufname) nil
			(not bang))))))

(evil-ex-define-cmd "wd[elete]" 'evil-write-delete-buffer)
(evil-ex-define-cmd "sf[ind]" 'evil-sfind)
(evil-ex-define-cmd "tabedit" 'evil-tabedit)

(define-key evil-normal-state-map (kbd "z <return>") 'enlarge-window)
(define-key evil-normal-state-map (kbd "g t") 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "g T") 'tab-bar-switch-to-prev-tab)

(provide 'my-evil)
;;; my-evil.el ends here
