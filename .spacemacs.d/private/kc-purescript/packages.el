;;; packages.el --- kc-purescript layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Christoph Hegemann<creek@arch>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `kc-purescript-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `kc-purescript/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `kc-purescript/pre-init-PACKAGE' and/or
;;   `kc-purescript/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst kc-purescript-packages
  '(
    (purescript-mode :location (recipe
                                :fetcher github
                                :repo "emacs-pe/purescript-mode"))
    (psc-ide :location (recipe
                        :fetcher github
                        :repo "kritzcreek/psc-ide-emacs"))
    (flycheck-purescript :location (recipe
                                    :fetcher github
                                    :repo "bsermons/flycheck-purescript"))
    psci
    company
    flycheck
    purescript-font-lock
    ))

(defun kc-purescript/post-init-company ()
  (spacemacs|add-company-hook purescript-mode))

(defun kc-purescript/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'purescript-mode-hook))

(defun kc-purescript/init-purescript-mode ()
  (use-package purescript-mode
    :defer t
    :config
    (progn
      (add-hook 'purescript-mode-hook 'haskell-indentation-mode)
      (setq haskell-literate nil))))

(defun kc-purescript/init-flycheck-purescript ()
  (use-package flycheck-purescript
    :defer t
    :if (configuration-layer/package-usedp 'flycheck)
    :config
    (progn
      (add-hook 'flycheck-mode-hook  'flycheck-purescript-setup)
      (add-hook 'purescript-mode-hook
                (lambda ()
                  (setq default-directory
                        (locate-dominating-file default-directory "bower.json")))))))

(defun kc-purescript/init-psc-ide ()
  (use-package psc-ide
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'psc-ide-mode)
      (push 'company-psc-ide-backend company-backends-purescript-mode)
      (spacemacs/declare-prefix-for-mode 'purescript-mode "mm" "purescript/psc-ide")
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "mt"  'psc-ide-add-clause
        "mcs" 'psc-ide-case-split
        "ms"  'psc-ide-server-start
        "ml"  'psc-ide-load-module
        "mia" 'psc-ide-add-import
        "ht"  'psc-ide-show-type))))

(defun kc-purescript/init-psci ()
  (use-package psci
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'psci 'psci "purescript")
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (spacemacs/set-leader-keys-for-major-mode 'purescript-mode
        "'"  'psci
        "sb" 'psci/load-current-file!
        "si" 'psci
        "sm" 'psci/load-module!
        "sp" 'psci/load-project-modules!))))

;;; packages.el ends here
