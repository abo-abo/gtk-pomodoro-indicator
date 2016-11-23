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

;; Integrate with `org-pomodoro-start' e.g. like this:
;;
;; (gtk-pomodoro-indicator
;;  (cl-case state
;;    (:pomodoro "p 25")
;;    (:short-break "b 5")
;;    (:long-break "b 20")
;;    (t (error "unexpected"))))

;;; Code:

(defvar gpi--process nil)

(defvar gpi--dir (file-name-directory load-file-name))

(defun gtk-pomodoro-indicator (params)
  (let ((cmd
         (format "python %s %s"
                 (expand-file-name "gtk-pomodoro-indicator.py" gpi--dir)
                 params)))
    (when (process-live-p gpi--process)
      (kill-process gpi--process))
    (setq gpi--process
          (start-process
           "pomodoro" nil
           shell-file-name shell-command-switch
           cmd))))

(provide 'gtk-pomodoro-indicator)
;;; gtk-pomodoro-indicator.el ends here
