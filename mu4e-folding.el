;;; mu4e-folding.el --- Thread folding support for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/mu4e-folding
;; Keywords: mail
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mu4e-folding.el is a library to enable threads folding in mu4e.
;; This works by using overlays with an invisible property and setting
;; hooks at the right place. It is possible to configure colors to
;; better highlight a thread. Note that when a thread is folded, any
;; unread child remains visible.
;;
;; Internally, the library works by creating overlay for each thread:
;; - mu4e-folding-root: overlay for the root of the thread
;; - mu4e-folding-all-children: overlay for all the thread children
;; - mu4e-folding-read-children: overlay for all the read children
;;
;; Children have two overlays such that we can hide the read children
;; while keeping the unread visible: when a thread is folded, unread
;; children staty visible
(require 'mu4e)

(defvar mu4e-folding--folded nil
  "Global folding state")


(defface mu4e-folding-root-unfolded-face
  `((t ))
  "Face for the thread root node when unfolded."
  :group 'mu4e-folding)


(defface mu4e-folding-root-folded-face
  `((t ))
  "Face for the thread root node when folded."
  :group 'mu4e-folding)


(defface mu4e-folding-child-folded-face
  `((t ))
  "Face for thread children when folded."
  :group 'mu4e-folding)


(defface mu4e-folding-child-unfolded-face
  `((t ))
  "Face for thread children when unfolded."
  :group 'mu4e-folding)


(defcustom mu4e-folding-default-view 'unfolded
  "Initial folding status ('folded or 'unfolded)."
  :type 'string
  :group 'mu4e-folding)


(defun mu4e-folding--message-at-point ()
  "Get message at point in mu4e headers mode"
  (get-text-property (point) 'msg))

(defun mu4e-folding--get-thread (msg)
  (when msg
    (if (version< mu4e-mu-version "1.8.0")
        (mu4e-message-field msg :thread)
      (mu4e-message-field msg :meta))))

(defun mu4e-folding--goto-root ()
  "Go to the root of the thread if message at point is part of a thread"

  (let* ((msg       (mu4e-folding--message-at-point))
         (thread    (mu4e-folding--get-thread msg))
         (orphan    (if thread (plist-get thread :orphan)))
         (root      (if thread (plist-get thread :root))))
    (when (and thread (not root) (not orphan) (not (bobp)))
      (forward-line -1)
      (mu4e-folding--goto-root))))


(defun mu4e-folding--is-root ()
  "Check if message at point is the root of a thread"
  (let* ((msg       (mu4e-folding--message-at-point))
         (thread    (mu4e-folding--get-thread msg))
         (has-child (if thread (plist-get thread :has-child)))
         (orphan    (if thread (plist-get thread :orphan)))
         (root      (if thread (plist-get thread :root))))
    (and thread has-child (or root orphan))))


(defun mu4e-folding--is-child ()
  "Check if message at point is a child in a thread"
  (let* ((msg       (mu4e-folding--message-at-point))
         (thread    (mu4e-folding--get-thread msg))
         (root      (if thread (plist-get thread :root))))
    (and thread (not root))))


(defun mu4e-folding--is-unread ()
  "Check if message at point is unread"
  (let ((msg (mu4e-folding--message-at-point)))
    (if msg (memq 'unread (mu4e-message-field msg :flags)))))


(defun mu4e-folding--is-folded ()
  "Check if message at point is folded"
  
  (save-excursion
    (let ((folded nil))
      (when (or (mu4e-folding--is-root)
                (mu4e-folding--is-child))
        (mu4e-folding--goto-root)
        (forward-line)
        (dolist (overlay (overlays-at (point)))
          (when (overlay-get overlay 'mu4e-folding-read-children)
            (setq folded (overlay-get overlay 'invisible)))))
      folded)))


(defun mu4e-folding--make-root-overlay (beg end)
  "Create the root overlay."

    (make-overlay beg end))

(defun mu4e-folding--make-children-all-overlay (beg end)
  "Create the all children overlay at point."
  
  (make-overlay beg end))

(defun mu4e-folding--make-children-read-overlay (beg end)
  "Create the read children overlay at"
  
  (make-overlay beg end))


(defun mu4e-folding--root-overlay (&optional force-create)
  "Get root overlay at point or create it."
  
  (let ((root-overlay nil))

    ;; Search for any existing root overlay
    (dolist (overlay (overlays-at (point)))
      (when (overlay-get overlay 'mu4e-folding-root-overlay)
        (if force-create
            (remove-overlays (overlay-start overlay)
                             (overlay-end overlay)
                             'mu4e-folding-root t)
          (setq root-overlay overlay))))

    ;; Create overlay if not found
    (unless root-overlay
      (setq root-overlay (mu4e-folding--make-root-overlay
                          (line-beginning-position)
                          (+ (line-end-position) 1)))
      (overlay-put root-overlay 'mu4e-folding-root-overlay root-overlay)
      (overlay-put root-overlay 'mu4e-folding-root t)
      (overlay-put root-overlay 'priority -200))
    root-overlay))


(defun mu4e-folding--children-overlay (&optional force-create)
  "Get children overlays at point or create them."

  (save-excursion
    (mu4e-folding--goto-root)

    (let ((all-children-overlay nil)
          (read-children-overlay nil))

      ;; Create overlays only if there is a thread root
      (when (mu4e-folding--is-root)
        (forward-line)
      
        ;; Search for overlays
        (dolist (overlay (overlays-at (point)))
          (when (overlay-get overlay 'mu4e-folding-all-children-overlay)
            (if force-create
              (remove-overlays (overlay-start overlay)
                               (overlay-end overlay)
                               'mu4e-folding-all-children t)
              (setq all-children-overlay overlay)))
          (when (overlay-get overlay 'mu4e-folding-read-children-overlay)
            (if force-create
                (remove-overlays (overlay-start overlay)
                                 (overlay-end overlay)
                                 'mu4e-folding-read-children t)
              (setq read-children-overlay overlay))))

        ;; Create overlays if they do not exist
        (unless all-children-overlay
          (let ((count 0)
                (start (point))
                (end-all (+ (point) 1))
                (end-read (+ (point) 1))
                (end-read-set nil))
        
            ;; Forward lines until we hit a non-child
            (while (and (< (point) (point-max))    ;; Stop before end of buffer
                        (mu4e-folding--is-child))  ;; Stop if non child
              (setq end-all (+ (line-end-position) 1))
              (if (mu4e-folding--is-unread)
                  (setq end-read-set t))
              (if (not end-read-set)
                  (setq end-read (+ (line-end-position) 1)))
              (setq count (+ count 1))
              (forward-line +1))

            ;; Create all-chidren overlay and set properties
            (setq all-children-overlay 
                  (mu4e-folding--make-children-all-overlay start end-all))
            (let ((overlay all-children-overlay))
              (overlay-put overlay 'face 'mu4e-folding-child-folded-face) 
              (overlay-put overlay 'priority -200)
              (overlay-put overlay 'mu4e-folding-children-count count)
              (overlay-put overlay 'mu4e-folding-all-children t)
              (overlay-put overlay 'mu4e-folding-all-children-overlay overlay))
          
            ;; Create read-chidren overlay and set properties
            (setq read-children-overlay
                  (mu4e-folding--make-children-read-overlay start end-read))
            (let ((overlay read-children-overlay))
              (overlay-put overlay 'priority -200)
              (overlay-put overlay 'mu4e-folding-read-children t)
              (overlay-put overlay 'mu4e-folding-read-children-overlay overlay)))))

      (cons all-children-overlay read-children-overlay))))


(defun mu4e-folding-fold-at-point ()
  "Fold thread at point (if any)"

  (interactive)
  (save-excursion
    (mu4e-folding--goto-root)
    (when (mu4e-folding--is-root)
      (let* ((overlay-root (mu4e-folding--root-overlay))
             (overlays (mu4e-folding--children-overlay))
             (overlay-all (car overlays))
             (overlay-read (cdr overlays)))
        (overlay-put overlay-read 'invisible t)
        (if (eq (overlay-end overlay-all) (overlay-end overlay-read))
            (overlay-put overlay-root 'face 'mu4e-folding-root-folded-face)
          (overlay-put overlay-root 'face 'mu4e-folding-root-unfolded-face))))))


(defun mu4e-folding-unfold-at-point ()
  "Unfold thread at point (if any)"

  (interactive)
  (save-excursion
    (mu4e-folding--goto-root)
    (when (mu4e-folding--is-root)
      (let ((overlay (mu4e-folding--root-overlay)))
;;        (overlay-put overlay 'face '(:background "#f0f0ff" :extend t)))
        (overlay-put overlay 'face 'mu4e-folding-root-unfolded-face))
      
      (let* ((overlays (mu4e-folding--children-overlay t))
             (overlay-all (car overlays))
             (overlay-read (cdr overlays)))
        (overlay-put overlay-read 'invisible nil)))))


(defun mu4e-folding-toggle-at-point ()
  "Toggle thread at point."
  
  (interactive)
  (save-excursion
    (if (mu4e-folding--is-folded)
        (mu4e-folding-unfold-at-point)
      (mu4e-folding-fold-at-point))))


(defun mu4e-folding-fold-all ()
  "Fold all threads independently of their current state."
  
  (interactive)
  (save-excursion
    (let ((roots '()))
      (goto-char (point-min))
      (while (not (eobp))
        (when (mu4e-folding--is-root)
          (push (point) roots))
        (forward-line))
      (dolist (root (reverse roots))
        (goto-char root)
        (mu4e-folding-fold-at-point))))
    (setq mu4e-folding--folded t))


(defun mu4e-folding-unfold-all ()
  "Unfold all threads, independently of their current state."
  (interactive)

  (save-excursion
    ;; Not strictly necessary but it allows to update overlays if
    ;; anything has changed.
    (mu4e-folding--remove-overlays)

    ;; This is necessary to keep overlay with unfolded faces
    (let ((roots '()))
      (goto-char (point-min))
      (while (not (eobp))
        (when (mu4e-folding--is-root)
          (push (point) roots))
        (forward-line))
      (dolist (root (reverse roots))
        (goto-char root)
        (mu4e-folding-unfold-at-point))))
  (setq mu4e-folding--folded nil))


(defun mu4e-folding-toggle-all ()
  "Toggle thread folding according to the global folding state."
  
  (interactive)
  (if mu4e-folding--folded
      (mu4e-folding-unfold-all)
    (mu4e-folding-fold-all)))


(defun mu4e-folding--remove-overlays ()
  "Delete all overlays."
  
  (remove-overlays (point-min) (point-max) 'mu4e-folding-root t)
  (remove-overlays (point-min) (point-max) 'mu4e-folding-all-children t)
  (remove-overlays (point-min) (point-max) 'mu4e-folding-read-children t))


(defun mu4e-folding--create-overlays ()
  "Create overlays for root and children."
  
  (save-excursion
    (let ((roots '()))
      (goto-char (point-min))

      ;; Find all thread roots
      (while (not (eobp))
        (when (mu4e-folding--is-root)
          (push (point) roots))
        (forward-line))

      ;; Create overlays
      (dolist (root (reverse roots))
        (goto-char root)
        (mu4e-folding--root-overlay t)
        (mu4e-folding--children-overlay t)))))


(defun mu4e-folding--apply ()
  "Apply folding according to the global folding state."

  (mu4e-folding--remove-overlays)
  (if mu4e-folding--folded
      (mu4e-folding-fold-all)
    (mu4e-folding-unfold-all)))


(defun mu4e-folding--activate ()
  "Activate folding mode (install overlays and hooks)"

  (if mu4e-folding-mode
      (mu4e-folding--apply))
  (add-hook 'mu4e-index-updated-hook #'mu4e-folding--apply)
  (add-hook 'mu4e-headers-found-hook #'mu4e-folding--apply)
  (add-hook 'mu4e-headers-mode-hook  #'mu4e-folding--apply))


(defun mu4e-folding--deactivate ()
  "Deactivate folding mode (remove overlays and hooks)"
  (mu4e-folding--remove-overlays)
  (remove-hook 'mu4e-index-updated-hook #'mu4e-folding--apply)
  (remove-hook 'mu4e-headers-found-hook #'mu4e-folding--apply)
  (remove-hook 'mu4e-headers-mode-hook  #'mu4e-folding--apply))


;;; ###autoload
(define-minor-mode mu4e-folding-mode
  "Minor mode for folding threads in mu4e-headers view."
  :init-value nil
  :group 'mu4e-folding
  :lighter "Folding"
  :global nil
  :keymap `((,(kbd "TAB") .       mu4e-folding-toggle-at-point)
            (,(kbd "<backtab>") . mu4e-folding-toggle-all))

  (if (derived-mode-p 'mu4e-headers-mode)
      (if mu4e-folding-mode
          (progn 
            (mu4e-folding--activate)
;;            (add-hook 'mu4e-index-updated-hook #'mu4e-folding-mode)
            (add-hook 'mu4e-headers-found-hook #'mu4e-folding-mode)
            (add-hook 'mu4e-headers-mode-hook  #'mu4e-folding-mode))
          (progn
            (mu4e-folding--deactivate)
;;            (remove-hook 'mu4e-index-updated-hook #'mu4e-folding-mode)
            (remove-hook 'mu4e-headers-found-hook #'mu4e-folding-mode)
            (remove-hook 'mu4e-headers-mode-hook  #'mu4e-folding-mode)))
      (progn
        (setq mu4e-folding-mode nil)
        (message
         "mu4e-folding mode can be only activated in mu4e-headers mode"))))

(provide 'mu4e-folding)
