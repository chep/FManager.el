;;; fmanager-el  ---  A file manager for emacs
;;
;; Filname: fmanager.el
;; Description: A file manager for emacs
;; Author: Cédric Chépied <cedric.chepied@gmail.com>
;; Maintainer: Cédric Chépied
;; Copyright (C) 2013, Cédric Chépied
;; Last updated: Tu Dec 10th
;;     By Cédric Chépied
;;     Update 1
;; Keywords: file manager 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Start the file manager with M-x fmanager and enter the path you want
;; to explore. It will create a new buffer and display files and
;; directories.
;;
;; Variables:
;;
;; A group fmanager is customizable with M-x customize-group
;;
;; fmanager-icon-theme: Icon theme for fmanager. Fmanager will try to find
;;                      icons in /usr/share/icons/'fmanager-icon-theme'
;; fmanager-item-max-width: max number of characters displayed for
;;                          file names
;; fmanager-item-spaces: Spaces between 2 fmanager items
;; fmanager-icon-size: Icon theme for fmanager. Fmanager will try to find
;;                     icons in
;;                     /usr/share/icons/'fmanager-icon-theme'/'fmanager-icon-size'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright Cédric Chépied 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'widget)

(defgroup fmanager nil "Customization of fmanager variables."
  :tag "fmanager"
  :group 'emacs)

(defcustom fmanager-icon-theme "oxygen" "Icon theme for fmanager"
  :tag "Icon theme for fmanager"
  :type 'string
  :group 'fmanager)

(defcustom fmanager-item-max-width 10 "Max width of a fmanager item"
  :tag "Max width of a fmanager item"
  :type 'integer
  :group 'fmanager)

(defcustom fmanager-item-spaces 5 "Spaces between 2 fmanager items"
  :tag "Spaces between 2 fmanager items"
  :type 'integer
  :group 'fmanager)

(defcustom fmanager-icon-size "64x64" "fmanager icon size"
  :tag "fmanager icon size"
  :type 'string
  :group 'fmanager)

(defcustom fmanager-show-hidden nil "Display hidden files"
  :tag "Display hidden files"
  :type 'boolean
  :group 'fmanager)

(defvar fmanager-images-table (make-hash-table)
  "Images table to store already loaded images")


(define-widget 'fmanager-file-widget 'push-button
  "File widget."
  :format         "%[%t%]"
  :button-face    'default
  :notify         (lambda (&rest ignore)
                           (message "Push button"))
  :create         'fmanager-file-widget-create)

(defun fmanager (directory)
  "Start fmanager"
  (interactive (list (read-directory-name "Directory: "
                                          "~")))
  (remove-overlays)
  (fmanager-display-directory (file-truename directory)))


(defun fmanager-display-directory (directory)
  "Create or open a buffer and display files in directory"
  (switch-to-buffer "*FManager*")
  (read-only-mode 0)
  (erase-buffer)
  (let ((spaces (/ (- (window-body-width) (length directory)) 2)))
    (setq-local header-line-format  (concat (make-string spaces ?\s) directory)))
  (newline)
  (beginning-of-buffer)
  (let ((files (fmanager-sort-files (directory-files directory) directory))
        (split (/ (1- (window-body-width))
                  (+ fmanager-item-max-width
                     fmanager-item-spaces)))
        (current 0))
    (dolist (f files)
      (insert-char ?\s (- (* current (+ fmanager-item-max-width
                                        fmanager-item-spaces))
                          (current-column)))
      (save-excursion
        (next-line)
        (end-of-line)
        (insert-char ?\s (- (* current (+ fmanager-item-max-width
                                         fmanager-item-spaces))
                           (current-column))))
      (widget-create 'fmanager-file-widget
                     :notify 'fmanager-open-file
                     :tag (concat directory "/" f)
                     :help-echo f)
      (setq current (1+ current))
      (when (>= current split)
          (progn (end-of-buffer)
                 (newline 2)
                 (previous-line)
                 (setq current 0)))))
  (read-only-mode 1)
  (beginning-of-buffer)
  (use-local-map widget-keymap))


(defun fmanager-file-widget-create(widget)
  "Create a fmanager widget. this is a button with text below"
  (let ((tag (widget-get widget :tag))
        (start (current-column))
        (image  (fmanager-find-image (widget-get widget :tag)))
        begin width)
    (setq width (truncate (car (image-size image))))
    (insert-char ?\s (/ (- fmanager-item-max-width width) 2))
    (widget-specify-insert
     (setq begin (point))
     (insert (widget-get-indirect widget :button-prefix))
     (insert-image image (make-string width ?\s))
     (widget-specify-button widget begin (point)))
    (save-excursion
      (next-line)
      (end-of-line)
      (widget-specify-insert
       (let ((name (file-name-nondirectory tag))
             nbChar)
         (setq nbChar (min fmanager-item-max-width (length name)))
         (insert-char ?\s (/ (- fmanager-item-max-width nbChar) 2))
         (insert (substring name
                            0
                            nbChar))
         (insert (widget-get-indirect widget :button-suffix)))))))


(defun fmanager-find-image (path)
  "Return icon image found with mime type."
  (let ((type (fmanager-get-mime-type path))
        icon)
    (setq icon (concat "/usr/share/icons/" fmanager-icon-theme
                       "/" fmanager-icon-size "/mimetypes/" type ".png"))
    (if (not (file-readable-p icon))
        (let ((type-generic (car (split-string type "-"))))
          (setq icon (concat "/usr/share/icons/" fmanager-icon-theme
                             "/" fmanager-icon-size "/mimetypes/"
                             type-generic "-x-generic.png"))
          (if (not (file-readable-p icon))
              (setq icon (concat "/usr/share/icons/" fmanager-icon-theme
                             "/" fmanager-icon-size
                             "/mimetypes/unknown.png")))))
    (let ((img (gethash icon fmanager-images-table nil)))
      (unless img
          (setq img (create-image icon))
          (puthash icon img fmanager-images-table))
      img)))

(defun fmanager-get-mime-type (path)
  "Return path mime type"
  (let ((type (replace-regexp-in-string "\n$" ""
                                        (shell-command-to-string (concat "file --mime-type "
                                                                         (shell-quote-argument path))))))
    (setq type (car (last (split-string type ":"))))
    (setq type (replace-regexp-in-string "^ " "" type))
    (setq type (replace-regexp-in-string "/" "-" type))
    type))


(defun fmanager-open-file (widget &rest ignore)
  "Open a file in emacs if file is text or use xdg-open"
  (let ((tag (widget-get widget :tag)))
    (when (file-readable-p tag)
      (if (file-directory-p tag)
          (fmanager-display-directory (file-truename tag))
        (let ((type (fmanager-get-mime-type tag)))
          (if (string-prefix-p "text" type)
              (find-file tag)
            (save-window-excursion (async-shell-command (concat "xdg-open "
                                                                (shell-quote-argument tag))
                                                        nil nil))))))))


(defun fmanager-sort-files (files directory)
  "Sort a list of files. Directories first and then regular files. Remove hidden files if fmanager-show-hidden is nil."
  (let ((directories)
        (other nil))
    (dolist (f files)
      (unless (or (string= f ".") (string= f "..")
                  (and (not fmanager-show-hidden) (string-prefix-p "." f)))
        (if (file-directory-p (concat directory "/" f))
            (setq directories (append directories (list f)))
            (setq other (append other (list f)))))
    )
    (setq directories (cl-sort directories 'string-lessp :key 'downcase))
    (setq other (cl-sort other 'string-lessp :key 'downcase))
  (append '("..") directories other)))

(provide 'fmanager)
