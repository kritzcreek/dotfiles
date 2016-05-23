(setq kc-org-journal-packages '(org-journal))

(defun kc-org-journal/init-org-journal ()
  (use-package org-journal
    :config
    (progn
      (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))
      (evil-leader/set-key
        "ajn" 'org-journal-new-entry
        "ajs" 'org-journal-search
        "ajd" 'org-journal-new-date-entry
        "ajr" 'org-journal-read-entry
        "ajj" 'org-journal-next-entry
        "ajk" 'org-journal-previous-entry
        "ajt" 'journal-file-today
        "ajy" 'journal-file-yesterday))))
