;;; exts.el --- Intro -*- lexical-binding: t -*-

;; Copyright 2023 Ramus Jabee Lloyd Wilson

;; Author: Ramus Jabee Lloyd Wilson
;; Maintainer: Ramus Jabee Lloyd Wilson
;; Created: 18 May 2023

;; Keywords: keywords
;; Homepage: "https://github.com/rwilson-lib/my-emacs-extensions"
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (package "0.1"))

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

;; Commentary
;;
;; Exported names start with "exts-"; private names start with "exts--".

;;; Code:

(require 'company)
(require 'yasnippet)

(defvar-local my/currrent-org-mode ""
  "The last org-src block mode the cursor visited in the buffer.")

(defun org-cursor-enter-src-block ()
  "Activate yas-extra-mode and company-mode when entering an org-src block.

  This function is called when the cursor enters an org-src block.  It
  activates yas-extra-mode if it's not already active and deactivates it
  when it's not needed anymore.  It also updates the current org-src block
  mode and activates company-mode if it's not already active."

  (if (org-in-src-block-p)
      (progn
	(let* ((lang (org-element-property :language (org-element-at-point)))
	       (lang-mode (cdr (assoc lang org-src-lang-modes)))
	       (lang-mode-str (if (null lang-mode) lang (concat (symbol-name lang-mode) "-mode")))
	       (lang-mode-str-with-mode
		(if (not (string-suffix-p "-mode" lang-mode-str))
		    (concat lang-mode-str "-mode") lang-mode-str ))
	       (mode (car (member (intern lang-mode-str) yas-extra-modes))))

	  (when (not (equal my/currrent-org-mode lang-mode-str-with-mode))
	    (yas-deactivate-extra-mode (intern my/currrent-org-mode))
	    (setq-local my/currrent-org-mode lang-mode-str-with-mode))

	  (when (null mode)
	    (yas-activate-extra-mode (intern lang-mode-str-with-mode))
	    (setq-local my/currrent-org-mode lang-mode-str-with-mode)))

	(when (not yas-minor-mode)
	  (yas-minor-mode 1))

	(when (not company-mode)
	  (company-mode 1)))

    (progn

      (when company-mode
	(company-mode -1))

      (when yas-minor-mode
	(yas-minor-mode -1))))) ; turn yas off

(add-hook 'post-command-hook #'org-cursor-enter-src-block nil t)
(add-hook 'evil-emacs-state-entry-hook #'org-cursor-enter-src-block nil t)

(provide 'org-my-ext)
;;; org-my-ext.el ends here

(setq-local whitespace-line-column 80)
(whitespace-mode)

(setq-local fill-column 80)
(display-fill-column-indicator-mode 1)


(defun append-to-scratch (beg end)
  (interactive "r")
  (append-to-buffer "*scratch*" beg end))


 (defcustom prompt-char "> "
  "The prompt character used by `insert-prompt'."
  :type 'string
  :group 'simple
  :safe #'stringp)

(defconst point-begin nil "Init prompt point")

(defun insert-prompt (&optional prompt)
  (interactive)
  (when (null (bolp))
    (goto-char (point-max))
    (insert "\n"))
  (let ((inhibit-read-only t)
	(insert-point (point-max)))
    (goto-char insert-point)
    (insert (or prompt prompt-char))
    (add-text-properties (1- insert-point) (point-max) '(read-only t
							 front-sticky (face read-only)
							 rear-nonsticky t))
    (goto-char (point-max))
    (defconst point-begin insert-point "The beginning of the buffer")))

(defface my-new-face
  '((t (:foreground "red" :background "yellow")))
  "My new face"
  :group 'basic-faces)


(defun submit-query ()
  (interactive)
  (let* ((inhibit-read-only t)
	 (start point-begin)
	 (end (point-max))
	 (value (buffer-substring start end)))
    (delete-region start end)
    (insert-query value)
    (insert-prompt)))

(defun process-return ()
  (interactive)
  (when (null point-begin) (insert-prompt))
  (if (< (point) point-begin)
      (goto-char (point-max))
    (submit-query)))

(defun insert-query (query)
  "Doc."
  (insert (propertize query 'read-only t 'font-lock-face 'my-new-face))
  (insert (propertize "\n\n" 'read-only t)))


(local-set-key (kbd "<RET>") 'process-return)

  ;; :hook
  ;; (org-mode . (lambda () 
  ;;               (setq-local yas-buffer-local-condition
  ;;                           '(not (org-in-src-block-p t)))))
  ;; (yas-global-mode 1))

;; (yas-activate-extra-mode 'fundamental-mode)))
;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")


     ;; :hook
    ;; (yas-minor-mode . (lambda ()
    ;; 		      (define-key yas-minor-mode-map (kbd "<tab>") nil)
    ;; 		      (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; 		      ;; Bind 'SPC' to 'yas-expand' when snippet expansion available (it
    ;; 		      ;; will still call 'self-insert-command' otherwise).
    ;; 		      (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
    ;; 		      ;; Bind `C-c y' to 'yas-expand' ONLY.
    ;; 		      (define-key yas-minor-mode-map
    ;;				    (kbd "C-c y") #'yas-expand)))

(provide 'exts)
;;; exts.el ends here
