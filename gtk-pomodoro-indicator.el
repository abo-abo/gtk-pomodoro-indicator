;;; gtk-pomodoro-indicator.el --- A pomodoro indicator for the GTK tray  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Oleh Krehel
;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/gtk-pomodoro-indicator
;; Version: 0.1.0
;; Keywords: convenience, pomodoro

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `gpi-setup' to integrate with `org-pomodoro'.

;;; Code:

(defvar gpi--process nil)

(defvar gpi--dir (file-name-directory (or load-file-name
                                          (buffer-file-name))))

;;;###autoload
(defun gtk-pomodoro-indicator (params)
  "Start the pomodoro timer with PARAMS and return the process.
PARAMS can be either: \"p NUMBER-MINUTES\" or \"b NUMBER-MINUTES\".
The only difference between the two is the icon type.
NUMBER-MINUTES is the number of minutes to count down from.
The timer will self-terminate after it expires."
  (let ((cmd
         (format "python %s %s"
                 (shell-quote-argument
                  (expand-file-name "pomodoro_indicator/pomodoro_indicator.py" gpi--dir))
                 params)))
    (when (process-live-p gpi--process)
      (kill-process gpi--process))
    (setq gpi--process
          (start-process
           "pomodoro" nil
           shell-file-name shell-command-switch
           cmd))))

(defun gpi--org-pomodoro-advice (orig-fun state)
  (gtk-pomodoro-indicator
   (cl-case state
     (:pomodoro "p 25")
     (:short-break "b 5")
     (:long-break "b 20")
     (t (error "unexpected"))))
  (funcall orig-fun state))

(defun gpi-setup (&optional turn-off)
  (require 'org-pomodoro)
  (if turn-off
      (advice-remove 'org-pomodoro-start 'gpi--org-pomodoro-advice)
    (advice-add 'org-pomodoro-start :around 'gpi--org-pomodoro-advice)))

(provide 'gtk-pomodoro-indicator)
;;; gtk-pomodoro-indicator.el ends here
