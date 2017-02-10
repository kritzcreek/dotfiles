;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private/")
   dotspacemacs-configuration-layers
   '(
     yaml
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete)
     emacs-lisp
     git
     markdown
     (osx :variables mac-right-option-modifier nil)
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-term-shell "/bin/zsh")
     ;; spell-checking
     (colors :variables colors-enable-nyan-cat-progress-bar t)
     syntax-checking
     latex
     html
     (haskell :variables haskell-completion-backend 'intero)
     ;; My personal layer
     purescript
     )
   dotspacemacs-additional-packages '(package-lint)
   dotspacemacs-excluded-packages '()
   dotspacemacs-line-numbers nil
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner "~/.spacemacs.d/purescript.png"
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '( material
                          material-light
                         )
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Operator Mono"
                               ;; :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key "SPC"
   dotspacemacs-visual-line-move-text t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 100
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  (setq custom-file "~/.spacemacs.d/custom.el")
  (load custom-file 'no-error)
  )

(defun dotspacemacs/user-config ()
  (add-to-list 'exec-path "~/.local/bin/")
  (setq org-default-notes-file "~/Documents/praxisprojekt/Dokumentation/notizen.org")

  ;; Fix ZSH bugs under OSX
  (setq system-uses-terminfo nil)

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 4
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  ;; Removes the strange highlighting of newly opened braces
  (setq sp-highlight-pair-overlay nil)

  ;; Sets the default browser for opening links to chrome
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium")

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; PSC-IDE
  (global-set-key (quote [f7]) 'psc-ide-add-clause)
  (global-set-key (quote [f8]) 'psc-ide-case-split)
  (global-set-key (kbd "C-SPC") 'company-complete)

  (defun purescript-unicodify ()
    "Query replaces the current buffer for unicode substitutions"
    (interactive)
    (when (eq major-mode 'purescript-mode)
      (save-excursion
        (beginning-of-buffer)
        (query-replace-regexp "->" "→")
        (query-replace-regexp "<-" "←")
        (query-replace-regexp "=>" "⇒")
        (query-replace-regexp "forall" "∀")
        (query-replace-regexp "::" "∷"))))

  (customize-set-variable 'psc-ide-rebuild-on-save nil)

)
