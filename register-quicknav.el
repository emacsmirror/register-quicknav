;;; register-quicknav.el --- Quickly jump to next/previous register.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tastytea

;; Author: tastytea <tastytea@tastytea.de>
;; Keywords: register
;; Version: 0.1.0

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

;; This package is built on top of register.el and allows you to quickly jump to
;; the next/previous position register.  If you reach the end, the search wraps
;; around and continues with the first (or last) register.

;; Features:
;;
;; * Cycle through all position registers in both directions.

;; Known limitations:
;;
;; Works only for as long as the buffer containing the registers is open.  If
;; you close and reopen it, it won't work anymore.

;; Installation:
;;
;; To use `register-quicknav.el', put it in your load-path and add the following
;; to your .emacs
;;
;; (require 'register-quicknav)
;; (global-set-key (kbd "<C-f5>") 'register-quicknav/prev-register)
;; (global-set-key (kbd "<C-f6>") 'register-quicknav/next-register)
;;
;; Or, with use-package:
;;
;; (use-package register-quicknav
;;   :commands (register-quicknav/prev-register register-quicknav/next-register)
;;   :bind
;;   ("C-<f5>" . register-quicknav/prev-register)
;;   ("C-<f6>" . register-quicknav/next-register))

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
    (sort result 'register-quicknav//sort-position-register-elements)))

(defun register-quicknav/next-register ()
  "Jump to next register."
  (interactive)
  (setq register-quicknav//current-position-register
        (+ register-quicknav//current-position-register 1))
  (when (>= register-quicknav//current-position-register
            (length (register-quicknav//registers)))
    (setq register-quicknav//current-position-register 0))
  (let (register-element register-name)
    (setq register-element
          (car (nthcdr register-quicknav//current-position-register
                       (register-quicknav//registers))))
    (setq register-name (car register-element))
    (register-to-point register-name)))

(defun register-quicknav/prev-register ()
  "Jump to previous register."
  (interactive)
  (setq register-quicknav//current-position-register
        (- register-quicknav//current-position-register 1))
  (when (< register-quicknav//current-position-register 0)
    (setq register-quicknav//current-position-register
          (- (length (register-quicknav//registers)) 1)))
  (let (register-element register-name)
    (setq register-element
          (car (nthcdr register-quicknav//current-position-register
                       (register-quicknav//registers))))
    (setq register-name (car register-element))
    (register-to-point register-name)))


(provide 'register-quicknav)
;;; register-quicknav.el ends here
