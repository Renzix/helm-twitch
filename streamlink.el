;;; streamlinkmlink.el --- A major mode for Streamlink output.

;; Copyright (C) 2015 Aaron Jacobs

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'easymenu)

(defgroup streamlink nil
  "A major mode for interacting with the streamlink program."
  :group 'external)

(defcustom streamlink-binary "streamlink"
  "The Streamlink binary path."
  :type 'string
  :group 'streamlink)

(defcustom streamlink-size "480p"
  "The stream size to request from Streamlink."
  :type 'string
  :group 'streamlink)

(defcustom streamlink-player nil
  "If non-nil, pass contents as the --player argument to
Streamlink."
  :type 'string
  :group 'streamlink)

(defcustom streamlink-opts nil
  "Additional options to pass to Streamlink."
  :type 'string
  :group 'streamlink)

(defcustom streamlink-header-fn #'streamlink-default-header
  "The function used to set the header-line."
  :type 'function
  :group 'streamlink)

(defcustom streamlink-header-fn-args nil
  "Arguments to pass `streamlink-header-fn'."
  :type 'list
  :group 'streamlink)

(defvar-local streamlink-process nil
  "The Streamlink process for a `streamlink-mode' buffer.")

(defvar-local streamlink-url nil
  "The current stream URL for a `streamlink-mode' buffer.")

(defvar-local streamlink-current-size nil
  "The current stream size for a `streamlink-mode' buffer.")

(defun streamlink-default-header ()
  "Provides the default header for `streamlink-mode'."
  (concat
   (propertize "Streamlink:" 'face 'font-lock-variable-name-face) " "
   (propertize streamlink-url 'face 'font-lock-constant-face) " "
   (propertize (concat "(" streamlink-current-size ")")
               'face 'font-lock-string-face)))

(defun streamlink-kill-buffer ()
  "Safely interrupt running stream players under Streamlink
before killing the buffer."
  (interactive)
  (when (eq major-mode 'streamlink-mode)
    (if (equal 'run (process-status streamlink-process))
        (interrupt-process streamlink-process)
      (kill-buffer))))

(defun streamlink-reopen-stream ()
  "Re-open the previous stream for this buffer using
Streamlink."
  (interactive)
  (when (eq major-mode 'streamlink-mode)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (if (equal 'run (process-status streamlink-process))
          ;; Don't try and re-open if the stream ain't closed!
          (insert (propertize
                   "# Cannot re-open stream: a stream is still open.\n"
                   'face 'font-lock-comment-face))
        (streamlink-open streamlink-url nil nil 'no-erase
                         "# Re-opening stream...\n")))))

(defun streamlink--get-stream-sizes ()
  "Retrieve the available stream sizes from the buffer's process
output content."
  (save-excursion
    (with-current-buffer "*streamlink*"
      (goto-char (point-min))
      (let* ((start (progn
                      (re-search-forward "Available streams:\s")
                      (point)))
             (end (progn (re-search-forward "$") (point))))
        (split-string
         (s-replace " (worst)" ""
                    (s-replace " (best)" ""
                               (buffer-substring start end))) ", ")))))

(defun streamlink-resize-stream (size)
  "Re-open the stream with a different size."
  (interactive
   (list (intern (completing-read
                  "Size: "
                  (streamlink--get-stream-sizes) nil t))))
  (when (eq major-mode 'streamlink-mode)
    (when (and (equal 'run (process-status streamlink-process))
               (y-or-n-p "Stream is currently open. Close it? "))
      (interrupt-process streamlink-process))
    (streamlink-open streamlink-url size nil 'no-erase)))

(defvar streamlink-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'streamlink-kill-buffer)
      (define-key map "r" 'streamlink-reopen-stream)
      (define-key map "s" 'streamlink-resize-stream)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)))
  "Keymap for `streamlink-mode'.")

(define-derived-mode streamlink-mode fundamental-mode "streamlink"
  "A major mode for Streamlink output."
  :group 'streamlink
  (setq truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode))

(defun streamlink-menu--add-sizes (default-menu)
  "Append a submenu to the DEFAULT-MENU listing the available
stream sizes."
  (let ((sizes (streamlink--get-stream-sizes)))
    (cons
     (cons "Resize stream (s)"
	   (mapcar
	    (lambda (size)
	      (vector size `(streamlink-resize-stream ,size)
		      :style 'radio
		      :selected `(equal ,size streamlink-current-size)))
	    sizes))
     default-menu)))

(easy-menu-define streamlink-menu streamlink-mode-map
  "Pop-up menu for `streamlink-mode'."
  '("Streamlink"
    :filter streamlink-menu--add-sizes
    ["Close stream" streamlink-kill-buffer
     ;; Only enable closing if the process is running. Otherwise
     ;; `streamlink-kill-buffer' will kill the buffer.
     :active (equal 'run (process-status streamlink-process))]
    ["Re-open stream" streamlink-reopen-stream
     ;; Only enable re-opening if the stream is not actually running.
     :active (not (equal 'run (process-status streamlink-process)))]))

(defun streamlink--filter (proc output)
  "Filter OUTPUT from Streamlink process PROC for display in
the buffer."
  (let ((buff (process-buffer proc)))
    (when (not (null buff))
      (with-current-buffer buff
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output))))))

(defun streamlink--sentinel (proc event)
  "Respond when Streamlink process PROC receives EVENT."
  (let ((buff (process-buffer proc)))
    (when (not (null buff))
      (with-current-buffer buff
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (if (equal event "finished\n")
              (insert
               (propertize
                "# Finished. Hit 'q' to kill the buffer or 'r' to re-open.\n"
                'face 'font-lock-comment-face))
            (insert (propertize (format "# Streamlink process had event: %s\n"
                                        event)
                                'face 'font-lock-comment-face))))))))

;;;###autoload
(defun streamlink-open (url &optional size opts no-erase msg)
  "Opens the stream at URL using the Streamlink program."
  (let* ((cmd  (executable-find streamlink-binary))
         (size (or size streamlink-size))
         (opts (or opts streamlink-opts ""))
         (opts (if streamlink-player
                   (concat streamlink-opts " --player \""
                           streamlink-player "\"")
                 streamlink-opts))
         (cmd  (when cmd (format "%s %s %s %s" cmd opts url size)))
         (buff (when cmd (get-buffer-create "*streamlink*")))
         (msg  (or msg "# Opening stream...\n")))
    (if cmd
        (with-current-buffer buff
          (switch-to-buffer buff)
          (unless (eq major-mode 'streamlink-mode)
            (streamlink-mode))
          (let ((inhibit-read-only t))
            (when (not no-erase)
              (erase-buffer))
            (insert (propertize msg 'face 'font-lock-comment-face))
            (let ((proc (start-process-shell-command cmd buff cmd)))
              (setq streamlink-process proc
                    streamlink-url url
                    streamlink-current-size size
                    header-line-format
                    `(:eval (funcall ',streamlink-header-fn
                                     ,@streamlink-header-fn-args)))
              (set-process-filter proc 'streamlink--filter)
              (set-process-sentinel proc 'streamlink--sentinel))
            nil))
      (message "Could not locate the streamlink program."))))

(provide 'streamlink)

;;; streamlink.el ends here
