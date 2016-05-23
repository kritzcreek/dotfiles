(defun get-journal-file-today ()
  "Return filename for today's journal entry"
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(defun get-journal-file-yesterday ()
  "Return filename for yesterday's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-yesterday ()
  "Creates and load a file based on yesterday's date."
  (interactive)
  (find-file (get-journal-file-yesterday)))

(defun bs/first-header ()
  (goto-char (point-min))
  (search-forward-regexp "^\* ")
  (beginning-of-line 1)
  (point))
