;;; side-hustle.el --- Navigate Imenu in a side window  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Paul W. Rankin

;; Author: Paul W. Rankin <pwr@bydasein.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.20"))
;; URL: https://git.bydasein.com/side-hustle

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

;;

;;; Code:

(require 'imenu)

(defgroup side-hustle nil
  "Navigate Imenu in a side window."
  :group 'convenience
  :group 'matching
  :link '(info-link "(emacs) Imenu"))


;;; Internal Variables

(defvar-local side-hustle--source-buffer nil)


;;; User Options

(defcustom side-hustle-display-alist
  '((side . left)
    (slot . 0)
    (window-width . 40))
  "Alist used to display side-hustle buffer."
  :type 'alist
  :group 'side-hustle
  :link '(info-link "(elisp) Buffer Display Action Alists"))

(defcustom side-hustle-select-window t
  "When non-nil, select the menu window after creating it."
  :type 'boolean
  :safe 'booleanp
  :group 'side-hustle)

(defcustom side-hustle-persistent-window nil
  "When non-nil, make the side-window persistent.
This requires either calling `quit-window' or
`side-hustle-toggle' to quit the side-window."
  :type 'boolean
  :safe 'booleanp
  :group 'side-hustle)

(defcustom side-hustle-evaporate-window nil
  "When non-nil, quit the side-window when calling
`side-hustle-goto-item'."
  :type 'boolean
  :safe 'booleanp
  :group 'side-hustle)

(defcustom side-hustle-indent-char ?\t
  "Character to use to indent levels of `imenu' items."
  :type 'character
  :safe 'characterp
  :group 'side-hustle)


;;; Faces

(defface side-hustle
  '((t (:underline nil :inherit button)))
  "Default face for side-window items."
  :group 'side-hustle)

(defface side-hustle-highlight
  '((t (:extend t :inherit (secondary-selection))))
  "Default face for highlighted items."
  :group 'side-hustle)


;;; Internal Functions

(defun side-hustle-insert (item level)
  "Insert ITEM at indentation level LEVEL.
And `imenu' marker as text property."
  (insert (make-string level side-hustle-indent-char))
  ;; (insert side-hustle-item-char "\s")
  (insert-text-button (car item)
                      'action #'side-hustle-goto-item
                      'face 'default
                      'help-echo "mouse-1, RET: Go to this item"
                      'follow-link t)
  (put-text-property (line-beginning-position)(line-end-position)
                     'side-hustle-imenu-marker (cdr item))
  (put-text-property (line-beginning-position) (line-end-position)
                     'side-hustle-level level)
  (insert ?\n))

(defun side-hustle-insert-items (imenu-items level)
  "For each item in IMENU-ITEMS, insert appropriately.
Either call `side-hustle-insert' at LEVEL, or if item is an
alist, insert alist string and increment LEVEL before calling
recursively with `cdr'."
  (mapc
   (lambda (item)
     (if (imenu--subalist-p item)
         (progn
           (side-hustle-insert item level)
           (side-hustle-insert-items (cdr item) (1+ level)))
       (side-hustle-insert item level)))
   imenu-items))

(defun side-hustle-refresh ()
  "Rebuild `imenu' entries for source buffer and insert entries."
  (interactive)
  (with-silent-modifications
    (let ((x (point))
          imenu-items)
      (when (buffer-live-p side-hustle--source-buffer)
        (setq imenu-items
              (with-current-buffer side-hustle--source-buffer
                (setq imenu--index-alist nil)
                (imenu--make-index-alist t)
                imenu--index-alist)))
      (erase-buffer)
      (setq header-line-format (buffer-name side-hustle--source-buffer))
      (when imenu-items (side-hustle-insert-items imenu-items 0))
      (goto-char x))))

(defun side-hustle-find-existing (src-buf)
  "Return existing `side-hustle' buffer for SRC-BUF or nil if none."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (eq side-hustle--source-buffer src-buf)))
   (buffer-list)))

(defun side-hustle-get-buffer-create (src-buf)
  "Get or create `side-hustle' buffer for SRC-BUF."
  (or (side-hustle-find-existing src-buf)
      (let ((new-buf (get-buffer-create
                      (concat "Side-Hustle: " (buffer-name src-buf)))))
        (with-current-buffer new-buf
          (side-hustle-mode)
          (setq side-hustle--source-buffer src-buf))
        new-buf)))

(defun side-hustle-highlight-current ()
  "Highlight the current `imenu' item in `side-hustle'.

Added to `window-configuration-change-hook'."
  (unless (or (minibuffer-window-active-p (selected-window))
              (eq major-mode 'side-hustle-mode))
    (let ((x (point))
          (buf (side-hustle-find-existing (current-buffer)))
          candidate diff)
      (when (and buf (window-live-p (get-buffer-window buf)))
        (with-current-buffer buf
          (with-silent-modifications
            (remove-text-properties (point-min) (point-max) '(face))
            (goto-char (point-min))
            (while (< (point) (point-max))
              (let ((marker (get-text-property (point) 'side-hustle-imenu-marker)))
                (when (and (markerp marker)
                           (<= (marker-position marker) x)
                           (or (null diff) (< (- x (marker-position marker)) diff)))
                  (setq candidate (point)
                        diff (if diff
                                 (min diff (- x (marker-position marker)))
                               (- x (marker-position marker))))))
              (forward-line 1))
            (when candidate
              (goto-char candidate)
              (put-text-property (line-beginning-position 1)
                                 (line-beginning-position 2)
                                 'face 'side-hustle-highlight))))))))


;;; Commands

(defun side-hustle-goto-item (&optional button)
  "Go to the `imenu' item at point."
  (interactive)
  (let ((buf (current-buffer))
        (marker (get-text-property (point) 'side-hustle-imenu-marker)))
    (when (markerp marker)
      (pop-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      ;; (side-hustle-highlight-current)
      (when side-hustle-evaporate-window
        (quit-window nil (get-buffer-window buf (selected-frame)))))))

(defun side-hustle-show-item ()
  "Display the `imenu' item at point in other window."
  (interactive)
  (let ((marker (get-text-property (point) 'side-hustle-imenu-marker)))
    (when (markerp marker)
      (save-selected-window
        (pop-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (recenter 0)))))

(defun side-hustle-show-hide ()
  "Show or hide the child items at point."
  (interactive)
  (let ((level (get-text-property (line-beginning-position) 'side-hustle-level))
        (start (line-end-position))
        (end (line-end-position)))
    (save-excursion
      (forward-line 1)
      (while (and (< (point) (point-max))
                  (< level (get-text-property (point) 'side-hustle-level)))
        (setq end (line-end-position))
        (forward-line 1)))
    (unless (= start end)
      (with-silent-modifications
        (if (eq (get-text-property (line-end-position) 'invisible)
                'side-hustle-invisible)
            (put-text-property start end 'invisible nil)
          (put-text-property start end 'invisible 'side-hustle-invisible))))))

;;;###autoload
(defun side-hustle-toggle ()
  "Pop up a side window containing `side-hustle'."
  (interactive)
  (if (eq major-mode 'side-hustle-mode)
      (quit-window)
    (let ((display-buffer-mark-dedicated t)
          (buf (side-hustle-get-buffer-create (current-buffer))))
      (if (get-buffer-window buf (selected-frame))
          (delete-windows-on buf (selected-frame))
        (display-buffer-in-side-window
         buf (append side-hustle-display-alist
                     (when side-hustle-persistent-window
                       (list '(window-parameters
                               (no-delete-other-windows . t))))))
        (with-current-buffer buf (side-hustle-refresh))
        ;; (side-hustle-highlight-current)
        (when side-hustle-select-window
          (select-window (get-buffer-window buf (selected-frame))))))))

(defalias 'toggle-side-hustle 'side-hustle-toggle)


;;; Mode Definition

(defvar side-hustle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'side-hustle-refresh)
    (define-key map (kbd "RET") #'side-hustle-goto-item)
    (define-key map (kbd "SPC") #'side-hustle-show-item)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "S-TAB") #'backward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map))

(define-derived-mode side-hustle-mode
  special-mode "Side-Hustle"
  "Major mode to navigate `imenu' via a side window."
  (add-to-invisibility-spec '(hustle-invisible . t)))



(provide 'side-hustle)
;;; side-hustle.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; fill-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
