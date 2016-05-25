(defconst kc-haskell-packages
  '(company
    flycheck
    haskell-mode
    (intero :location (recipe
                       :repo "chrisdone/intero"
                       :fetcher github
                       :files ("elisp/intero.el")))))

(defun kc-haskell/post-init-company ()
  (add-hook 'intero-mode-hook 'company-mode))

(defun kc-haskell/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'intero-mode-hook))

(defun kc-haskell/init-haskell-mode ()
  (use-package haskell-mode))

(defun kc-haskell/post-init-haskell-mode ()
  (add-hook 'haskell-mode-hook 'intero-mode))

(defun kc-haskell/init-intero ()
  (use-package intero))
