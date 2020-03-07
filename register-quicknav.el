;;; register-quicknav.el --- Quickly jump to next/previous register -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tastytea

;; Author: tastytea <tastytea@tastytea.de>
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; URL: https://schlomp.space/tastytea/register-quicknav

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is built on top of `register.el' and allows you to quickly jump
;; to the next/previous position register.  If you reach the end, the search
;; wraps around and continues with the first (or last) register.

;; Features:
;;
;; * Cycle through all position registers in both directions.
;; * Clear current register.

;; Installation:
;;
;; **Note:** The function and variable names were previously separated by “/”
;; from the namespace.  To conform with MELPA rules the separator has been
;; changed to “-”.
;;
;; To use `register-quicknav.el', put it in your load-path and add the following
;; to your init.el:
;;
;; (require 'register-quicknav)
;; (global-set-key (kbd "<C-f5>") #'register-quicknav-prev-register)
;; (global-set-key (kbd "<C-f6>") #'register-quicknav-next-register)
;; (global-set-key (kbd "M-r")    #'register-quicknav-clear-current-register)
;;
;; Or, with use-package:
;;
;; (use-package register-quicknav
;;   :bind (("C-<f5>" . register-quicknav-prev-register)
;;          ("C-<f6>" . register-quicknav-next-register)
;;          ("M-r"    . register-quicknav-clear-current-register)))
;;
;; Instead of manually copying `register-quicknav.el' into your load-path, you
;; can use [quelpa](https://github.com/quelpa/quelpa):
;;
;; (quelpa '(register-quicknav
;;           :fetcher git
;;           :url "https://schlomp.space/tastytea/register-quicknav.git"))

;; Variables:
;;
;; * `register-quicknav-buffer-only': Cycle only through position registers in
;;   current buffer.

;;; Code:

(require 'cl-lib)

(define-obsolete-variable-alias 'register-quicknav/buffer-only
  'register-quicknav-buffer-only "0.1.1")
(define-obsolete-function-alias 'register-quicknav/next-register
  #'register-quicknav-next-register "0.1.1")
(define-obsolete-function-alias 'register-quicknav/prev-register
  #'register-quicknav-prev-register "0.1.1")
(define-obsolete-function-alias 'register-quicknav/clear-current-register
  #'register-quicknav-clear-current-register "0.1.1")

(defgroup register-quicknav nil
  "Variables for register-quicknav."
  :group 'editing)

(defcustom register-quicknav-buffer-only nil
  "Cycle only through position registers in current buffer."
  :type 'boolean
  :group 'register-quicknav)

(defvar register-quicknav--current-position-register 0
  "An index to the current position register.")

(defun register-quicknav--registers ()
  "Return all position registers, sorted by file name and position.
If `register-quicknav-buffer-only' is t, return only registers in
current buffer."
  (cl-flet* ((item-file-name
              (lambda (item)
                "Return file-name of ITEM.
Works on markers and file-queries."
                (if (markerp (cdr item))
                    (buffer-file-name (marker-buffer (cdr item)))
                  (nth 2 item))))
             (is-current-buffer?
              (lambda (item)
                "Return t if ITEM is in current buffer.
Works on markers and file-queries."
                (if (markerp (cdr item))
                    (eq (current-buffer) (marker-buffer (cdr item)))
                  (string= (buffer-file-name (current-buffer))
                           (item-file-name item)))))
             (sort-registers
              (lambda (a b)
                "Return t if position of A is < B.
Works on markers and file-queries."
                (cl-flet ((item-position
                           (lambda (item)
                             "Return position of ITEM.
Works on markers and file-queries."
                             (if (markerp (cdr item))
                                 (marker-position (cdr item))
                               (nth 3 item)))))
                  (and (string= (item-file-name a)
                                (item-file-name b))
                       (< (item-position a)
                          (item-position b)))))))
    (let ((result))
      (dolist (item register-alist)
        (if (or (markerp (cdr item))
                (eq (nth 1 item) 'file-query))
            (if register-quicknav-buffer-only
                (when (is-current-buffer? item)
                  (push item result))
              (push item result))))
      (sort result #'sort-registers))))

;;;###autoload
(defun register-quicknav-next-register ()
  "Jump to next position register."
  (interactive)
  (let ((pos register-quicknav--current-position-register)
        (registers (register-quicknav--registers)))
    (cl-incf pos)
    (when (>= pos (length registers))
      (setq pos 0))
    (setq register-quicknav--current-position-register pos)
    (register-to-point (car (nth pos registers)))))

;;;###autoload
(defun register-quicknav-prev-register ()
  "Jump to previous position register."
  (interactive)
  (let ((pos register-quicknav--current-position-register)
        (registers (register-quicknav--registers)))
    (cl-decf pos)
    (when (< pos 0)
      (setq pos (- (length registers) 1)))
    (setq register-quicknav--current-position-register pos)
    (register-to-point (car (nth pos registers)))))

;;;###autoload
(defun register-quicknav-clear-current-register ()
  "Clear currently selected position register.
To be more precise, it deletes the
`register-quicknav--current-position-register'th position
register, as reported by `register-quicknav--registers', from
`register-alist'."
  (interactive)
  (let ((pos register-quicknav--current-position-register)
        (registers (register-quicknav--registers)))
    (setq register-alist (delq (nth pos registers) register-alist))))

(provide 'register-quicknav)
;;; register-quicknav.el ends here
