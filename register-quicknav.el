;;; register-quicknav.el --- Quickly jump to next/previous register.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tastytea

;; Author: tastytea <tastytea@tastytea.de>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))
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

;; Known limitations:
;;
;; Works only for as long as the buffer containing the registers is open.  If
;; you close and reopen it, it won't work anymore.

;; Installation:
;;
;; To use `register-quicknav.el', put it in your load-path and add the following
;; to your init.el:
;;
;; (require 'register-quicknav)
;; (global-set-key (kbd "<C-f5>") #'register-quicknav/prev-register)
;; (global-set-key (kbd "<C-f6>") #'register-quicknav/next-register)
;; (global-set-key (kbd "M-r")    #'register-quicknav/clear-current-register)
;;
;; Or, with use-package:
;;
;; (use-package register-quicknav
;;   :bind (("C-<f5>" . register-quicknav/prev-register)
;;          ("C-<f6>" . register-quicknav/next-register)
;;          ("M-r"    . register-quicknav/clear-current-register)))

;; Tips:
;;
;; To only cycle through the registers of the current buffer, add
;;     (make-variable-buffer-local 'register-alist)
;; to your init.el.

;;; Code:

(defvar register-quicknav//current-position-register 0
  "An index to the current position register.")

(defun register-quicknav//sort-position-register-elements (a b)
  "Return t if the file name is the same and A < B."
  (let ((marker-a (cdr a))
        (marker-b (cdr b)))
    (and (string= (buffer-file-name (marker-buffer marker-a))
                  (buffer-file-name (marker-buffer marker-b)))
         (< (marker-position marker-a)
            (marker-position marker-b)))))

(defun register-quicknav//registers ()
  "Return all position registers, sorted by file name and position."
  (let (result)
    (dolist (item register-alist)
      (when (markerp (cdr item))
        (setq result (cons item result))))
    (sort result #'register-quicknav//sort-position-register-elements)))

(defun register-quicknav/next-register ()
  "Jump to next position register."
  (interactive)
  (let ((pos register-quicknav//current-position-register)
        (registers (register-quicknav//registers)))
    (setq pos (+ pos 1))
    (when (>= pos (length registers))
      (setq pos 0))
    (setq register-quicknav//current-position-register pos)
    (register-to-point (car (nth pos registers)))))

(defun register-quicknav/prev-register ()
  "Jump to previous position register."
  (interactive)
  (let ((pos register-quicknav//current-position-register)
        (registers (register-quicknav//registers)))
    (setq pos (- pos 1))
    (when (< pos 0)
      (setq pos (- (length registers) 1)))
    (setq register-quicknav//current-position-register pos)
    (register-to-point (car (nth pos registers)))))

(defun register-quicknav/clear-current-register ()
  "Clear currently selected position register.
To be more precise, it deletes the
`register-quicknav//current-position-register'th position
register, as reported by `register-quicknav//registers', from
`register-alist'."
  (interactive)
  (let ((pos register-quicknav//current-position-register)
        (registers (register-quicknav//registers)))
    (setq register-alist (delete (nth pos registers) register-alist))))


(provide 'register-quicknav)
;;; register-quicknav.el ends here
