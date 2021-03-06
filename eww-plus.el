;; eww-plus.el --- Some helper functions for EWW.  -*- lexical-binding: t; -*-

;; Filename: eww-plus.el
;; Description: Some helper functions for EWW.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2021, zbelial, all rights reserved.
;; Created: 2021-02-21 14:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/eww-plus.el
;; Package-Requires: ((ivy "0.13.0"))
;; Keywords:
;; Compatibility: GNU Emacs 27.1
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
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

(require 'ivy)
(require 'eww)

(defgroup eww-plus nil
  "Browse files in Emacs using ivy."
  :prefix "eww-plus-" :group 'eww)

;;; Custom
(defcustom eww-plus-session-file (concat user-emacs-directory "eww-session.el")
  "The default directory used to store topic thread data."
  :type  'string
  :group 'eww-plus)

(defcustom eww-plus-expire-time -1
  "Files with these extensions will be listed."
  :type 'integer
  :group 'eww-plus)

;;; Variables
(defvar eww-plus-position-alist '()
  "Keep track of urls and postions when killed, plus timestamps.")

(defvar eww-plus--recent-timestamp 0
  "The most recent time when saving the session.")

;;; Functions
(defun eww-plus--file-modified-time (file)
  "File's latest modification time."
  (if (f-exists-p file)
      (time-convert (file-attribute-modification-time (file-attributes file)) 'integer)
    0))

(defun eww-plus--session-file-modified? ()
  "Check whether `eww-plus-session-file' is modified by external actions."
  (> (eww-plus--file-modified-time eww-plus-session-file) eww-plus--recent-timestamp))

(defun eww-plus--update-recent-timestamp (ts)
  "Update `eww-plus--recent-timestamp'."
  (setq eww-plus--recent-timestamp ts))

(defun eww-plus--now()
  "Current timestamp."
  (time-convert nil 'integer))

(defun eww-plus--save-session()
  "Save eww session when kill emacs."
  (with-temp-file eww-plus-session-file
    (erase-buffer)
    (insert ";;; -*- mode: emacs-lisp -*-\n")
    (when eww-plus-position-alist
      (setq eww-plus-position-alist (sort eww-plus-position-alist (lambda (p1 p2) (string-lessp (car p1) (car p2)))))
      (insert (format "\(setq eww-plus-position-alist '"))
      (insert (format "%S\n" eww-plus-position-alist))
      (insert (format "\)\n")))))

(defun eww-plus-kill-buffer-hook ()
  "Save position"
  (when (derived-mode-p 'eww-mode)
    (eww-plus--maybe-restore)
    (let ((url (eww-current-url))
          (position (line-number-at-pos))
          (timestamp (eww-plus--now)))
      (if (assoc url eww-plus-position-alist)
          (setcdr (assoc url eww-plus-position-alist) `(,position . ,timestamp))
        (add-to-list 'eww-plus-position-alist `(,url . (,position . ,timestamp)) t)))
    (eww-plus--save-session)
    (eww-plus--update-recent-timestamp (eww-plus--now))))

(defun eww-plus-save-buffer-position ()
  "Save current position into session file."
  (interactive)
  (eww-plus-kill-buffer-hook)
  )

(defun eww-plus-save-session-hook ()
  "Save position of all eww buffers."
  (eww-plus--maybe-restore)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'eww-mode)
        (let ((url (eww-current-url))
              (position (line-number-at-pos))
              (timestamp (eww-plus--now)))
          (if (assoc url eww-plus-position-alist)
              (setcdr (assoc url eww-plus-position-alist) `(,position . ,timestamp))
            (add-to-list 'eww-plus-position-alist `(,url . (,position . ,timestamp)) t)))
        )))
  (eww-plus--save-session))

(defun eww-plus-restore-position-hook()
  "Restore postion for a eww buffer."
  (run-with-timer 0.2 nil (lambda ()
                            (let* ((url (eww-current-url))
                                   (record (assoc url eww-plus-position-alist))
                                   (position 1))
                              (when record
                                (setq position (cadr record)))
                              (goto-char (point-min))
                              (forward-line (1- position))
                              (recenter)))))

(defun eww-plus--maybe-restore ()
  "If needed, reload session files."
  (when (eww-plus--session-file-modified?)
    (eww-plus-restore-session-hook)))

(defun eww-plus-restore-session-hook()
  "Restore eww session."
  (setq eww-plus-position-alist '())
  (when (file-exists-p eww-plus-session-file)
    (eww-plus--update-recent-timestamp (eww-plus--file-modified-time eww-plus-session-file))

    (load-file eww-plus-session-file)
    (when (/= eww-plus-expire-time -1)
      (setq eww-plus-position-alist (seq-filter (lambda (r) (>= (+ (cddr r)
                                                                   (* eww-plus-expire-time 24 3600))
                                                                (eww-plus--now)))
                                                eww-plus-position-alist)))))

(defun eww-plus--visited-url-sorter (u1 u2)
  "Sorting function for `eww-plus-list-visited-urls'."
  (let ((u1t (cddr (cdr u1)))
        (u2t (cddr (cdr u2))))
    (> u1t u2t)))

(defun eww-plus--visited-url-collector ()
  "Retrieve all visited urls."
  (eww-plus--maybe-restore)
  (when eww-plus-position-alist
    (let (urls url tm)
      (dolist (p eww-plus-position-alist)
        (cl-pushnew (cons (format "%-120s%s" (car p) (format-time-string "%Y-%m-%d %H:%M:%S" (cddr p))) p) urls))
      (cl-sort urls #'eww-plus--visited-url-sorter))))

(defun eww-plus-switch-to-or-open (url)
  "Open url or switch to the buffer that opens url."
  (interactive)
  (let ((buffers (buffer-list))
        target)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (and
               (derived-mode-p 'eww-mode)
               (equal url (eww-current-url)))
          (setq target buffer))))
    (if target
        (switch-to-buffer target)
      (eww url 4))))

(defun eww-plus-list-visited-urls()
  "Show all visited urls using ivy."
  (interactive)
  (let ((urls (eww-plus--visited-url-collector)))
    (ivy-read "visited URLs: " urls
              :action (lambda (url) (eww-plus-switch-to-or-open (cadr url)))
              )
    ))

(defun eww-plus-list-buffers()
  "List eww buffers."
  (interactive)
  (let (buffers-info
        (domain-length 0)
        (title-length 0)
        url title format
        buffers
        )
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eww-mode)
          (push (vector buffer (plist-get eww-data :title)
                        (plist-get eww-data :url))
                buffers-info))))

    (setq buffers-info (nreverse buffers-info)) ;more recent on top

    (dolist (buffer-info buffers-info)
      (setq title-length (max title-length
                              (length (elt buffer-info 1)))
            domain-length (max domain-length
                               (length (elt buffer-info 2)))))
    
    (setq format (format "%%-%ds %%-%ds" title-length domain-length))

    (dolist (buffer-info buffers-info)
      (setq buffer (elt buffer-info 0)
            title (elt buffer-info 1)
            url (elt buffer-info 2))
      (cl-pushnew (cons (format format title url) buffer) buffers))

    (ivy-read "EWW buffers: " buffers
              :action #'(lambda (buf)
                          (switch-to-buffer (cdr buf)))
              :caller 'eww-plus-list-buffers)
    ))

(defun eww-plus-eww-reload-before-advice (&optional LOCAL ENCODE)
  (eww-plus-kill-buffer-hook)
  )

(defvar eww-plus-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `eww-plus-mode'.")


;;;###autoload
(define-minor-mode eww-plus-mode
  "Toggle Eww-Plus mode on or off.
Turn Eww-Plus mode on if ARG is positive, off otherwise.

Global bindings:
\\{eww-plus-mode-map}
"
  :group 'eww-plus
  :global t
  :keymap eww-plus-mode-map
  :lighter " eww-plus"
  (if eww-plus-mode
      (progn
        (add-hook 'kill-buffer-hook #'eww-plus-kill-buffer-hook)
        (add-hook 'eww-after-render-hook #'eww-plus-restore-position-hook)
        (add-hook 'after-init-hook #'eww-plus-restore-session-hook)
        (add-hook 'kill-emacs-hook #'eww-plus-save-session-hook)
        (advice-add 'eww-reload :before #'eww-plus-eww-reload-before-advice)
        )
    (remove-hook 'kill-buffer-hook #'eww-plus-kill-buffer-hook)
    (remove-hook 'eww-after-render-hook #'eww-plus-restore-position-hook)
    (remove-hook 'after-init-hook #'eww-plus-restore-session-hook)
    (remove-hook 'kill-emacs-hook #'eww-plus-save-session-hook)
    (advice-remove 'eww-reload #'eww-plus-eww-reload-before-advice)
    ))

(provide 'eww-plus)
