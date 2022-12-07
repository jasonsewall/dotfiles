(require 'early-init (expand-file-name "early-init" user-emacs-directory))

(straight-use-package 'use-package)

(server-start)

(setq inhibit-startup-message t)

(setq user-full-name "Jason Sewall"
      user-mail-address "jasewall@nvidia.com")

(setq calendar-latitude 43.8951048
      calendar-longitude -69.525429740
      calendar-location-name "Pemaquid, ME")

(setq calendar-time-zone (* 60 5)
      calendar-standard-time-zone-name "EST"
      calendar-daylight-time-zone-name "EDT")

(setq use-package-verbose t)
(use-package auto-compile
  :straight t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package hydra
:straight t)

(use-package flycheck
  :straight t
  :diminish
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clang-analyzer
  :straight t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package key-chord
  :straight t
  :init (key-chord-mode 1))

(use-package dash
  :straight t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(setq uniquify-buffer-name-style 'reverse
      uniquify-after-kill-buffer-p t)

(desktop-save-mode 1)
(setq desktop-restore-eager 10)

(defhydra hydra-desktop (:color blue)
  "desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "dir"))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(when window-system
  (global-unset-key "\C-z"))
(when window-system
  (global-unset-key "\C-x\C-z"))

(column-number-mode 1)
(display-time)

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c\C-v" 'save-buffers-kill-emacs)

(setq frame-title-format
      (concat  "emacs@" (system-name)))
(unless window-system
  (send-string-to-terminal (concat "\ek" frame-title-format "\e\\")))

(use-package ace-window
  :straight t)
(defhydra hydra-window (global-map "C-x w")
  "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_h_ ←         _v_ertical      _b_uffer        _q_ X←
_j_ ↓         _x_ horizontal  _f_ind files    _w_ X↓
_k_ ↑         _z_ undo        _a_ce 1     _e_ X↑
_l_ →         _Z_ reset       _s_wap      _r_ X→
_F_ollow        _D_lt Other     _S_ave      max_i_mize
_SPC_ cancel    _o_nly this     _d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("F" follow-mode)
  ("a" (lambda ()
	 (interactive)
	 (ace-window 1)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body))
   )
  ("v" (lambda ()
	 (interactive)
	 (split-window-right)
	 (windmove-right))
   )
  ("x" (lambda ()
	 (interactive)
	 (split-window-below)
	 (windmove-down))
   )
  ("s" (lambda ()
	 (interactive)
	 (ace-window 4)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
	 (interactive)
	 (ace-window 16)
	 (add-hook 'ace-window-end-once-hook
		   'hydra-window/body))
   )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
	 (winner-undo)
	 (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("SPC" nil))

;;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))

(use-package orderless
  :straight t
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            (setq completion-styles '(orderless))

            (defun vifon/orderless-without-if-bang (pattern index total)
              (when (string-prefix-p "!" pattern)
                `(orderless-without-literal . ,(substring pattern 1))))
            (defun vifon/orderless-literal-if-equal (pattern index total)
              (when (string-suffix-p "=" pattern)
                `(orderless-literal . ,(substring pattern 0 -1))))
            (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                                vifon/orderless-literal-if-equal))))

(use-package embark
  :straight t
  :bind (("C-c o" . embark-act)
         ("C-."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         :map embark-command-map
         ;; Unbind the dangerous `global-set-key' and `local-set-key'
         ;; actions.  It's far too easy to accidentally bind over some
         ;; `self-insert-command' binding or even over
         ;; \\[keyboard-quit].
         ("g" . nil)
         ("l" . nil))
  :config (progn
            (setq embark-mixed-indicator-delay 2)

            ;; Make the eval action editable.  Evaluating code
            ;; in-place is simple enough without Embark, if I invoke
            ;; it with Embark, I almost definitely want to edit the
            ;; expression beforehand.  And even if not, I can
            ;; just confirm.
            (cl-pushnew 'embark--allow-edit
                        (alist-get 'pp-eval-expression embark-target-injection-hooks))

            ;; Reload the project list after using
            ;; C-u `embark-act' with `project-forget-project'.
            (cl-pushnew 'embark--restart
                        (alist-get 'project-forget-project embark-post-action-hooks))

            (defun embark-act-with-eval (expression)
              "Evaluate EXPRESSION and call `embark-act' on the result."
              (interactive "sExpression: ")
              (with-temp-buffer
                (let ((expr-value (eval (read expression))))
                  (insert (if (stringp expr-value)
                              expr-value
                            (format "%S" expr-value))))
                (embark-act)))

            (dolist (keymap (list embark-variable-map embark-expression-map))
              (define-key keymap (kbd "v") #'embark-act-with-eval))

            ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
            (autoload 'gnus-dired-attach "gnus-dired" nil t)
            (defun embark-attach-file (file)
              "Attach FILE to an email message."
              (interactive "fAttach: ")
              (gnus-dired-attach (list file)))
            (bind-key "a" #'embark-attach-file embark-file-map)))

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package marginalia
  :straight t
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :straight t
  :bind (("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ([remap previous-matching-history-element] . consult-history)
         :map isearch-mode-map
         ("TAB" . vifon/isearch-to-consult-line))
  :config (progn
            (setq consult-project-root-function #'vc-root-dir)
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key (kbd "M-."))

            (defun vifon/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
                `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x200000-\x300000]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (defun vifon/isearch-to-consult-line ()
              "Search using `consult-line' what was being searched with `isearch'."
              (interactive)
              (isearch-exit)
              (let ((query (if isearch-regexp
                               isearch-string
                             (regexp-quote isearch-string))))
                (consult-line query)))))

(use-package corfu
  :straight t
  :init (global-corfu-mode 1))


;;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
(autoload 'ffap-file-at-point "ffap")
(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table :exclusive 'no))))
          'append)

;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;;
;;; Taken from the Vertico docs.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion unless
;;; overridden (for example by `corfu').
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))

(use-package smart-mode-line
  :straight t)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :straight t
  :commands minibuffer-edit
  :init (miniedit-install))

(use-package zenburn-theme
  :straight t
  :init
  (progn
    (cond
     (window-system (load-theme 'zenburn t))
     (t             (load-theme 'zenburn t)))))

(setq transient-mark-mode t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(show-paren-mode t)

(use-package guide-key
  :straight t
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))  ; Enable guide-key-mode

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(define-coding-system-alias 'UTF-8 'utf-8)
(setq read-quoted-char-radix 16)

(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq sentence-end-double-space t)

(bind-key "M-/" 'hippie-expand)

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(use-package diminish
  :straight t
  :init (diminish 'eldoc-mode))

(use-package powerline
  :straight t
  :config (powerline-default-theme))

(use-package tramp
  :straight (:type built-in)
  :init (setq tramp-unified-filename t))

(use-package tramp-term
  :straight t)

(add-hook 'term-mode-hook
	  (lambda ()
	    (setq term-buffer-maximum-size 100000)))

;; Use this for remote so I can specify command line arguments
(defun my/remote-term (new-buffer-name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (let (term-escape-char)
    ;; I wanna have find-file on C-x C-f -mm
    ;; your mileage may definitely vary, maybe it's better to put this in your
    ;; .emacs ...
    (term-set-escape-char ?\C-x))

  (switch-to-buffer term-ansi-buffer-name))

(use-package pcomplete
  :straight t
  :init (progn (require 'pcmpl-unix) (defun my/ssh-remote-term (hostname)
				       (interactive (list (completing-read "Hostname: " (pcmpl-ssh-hosts))))
				       (my/remote-term hostname "ssh" hostname))))

;; (defun helm-source-ssh-remote-term ()
;;   (helm-build-sync-source "SSH hostname"
;;     :candidates (lambda () (pcmpl-ssh-hosts))
;;     :filtered-candidate-transformer '(helm-adaptive-sort)
;;     :nomark t
;;     :action '(("Select host" . my/ssh-remote-term))))

;; (defun my/helm-ssh-remote-term ()
;;   (interactive)
;;   (helm :sources (helm-source-ssh-remote-term)
;;         :buffer "*helm-ssh-remote-term*"))

(defun my/local-term ()
  (interactive)
  (ansi-term "bash" "localhost"))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(use-package ibuffer
  :straight t
  :bind (("<f9>" . ibuffer))
  :init (setq ibuffer-shrink-to-minimum-size t
	      ibuffer-always-show-last-buffer nil
	      ibuffer-sorting-mode 'recency
	      ibuffer-use-header-line t))

(add-to-list 'vc-handled-backends 'GIT)

(setq vc-follow-symlinks t)

(use-package git-link
  :straight t
  :init (global-set-key (kbd "C-c m l") 'git-link))

(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))

(setq diff-switches "-u")
(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

(use-package magit
  :straight t
  :init (setq magit-auto-revert-mode t)
  :bind (("C-x C-g" . magit-status)))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules"))
  :config
  (projectile-global-mode))

(global-set-key "\C-cg" 'goto-line)

(use-package avy
  :straight t
  :init (defhydra hydra-avy (global-map "M-g" :color blue)
	  "avy-goto"
	  ("c" avy-goto-char "char")
	  ("C" avy-goto-char-2 "char-2")
	  ("w" avy-goto-word-1 "word")
	  ("s" avy-goto-subword-1 "subword")
	  ("u" link-hint-open-link "open-URI")
	  ("U" link-hint-copy-link "copy-URI"))
  :bind (("M-g g" . avy-goto-line)))

(defhydra hydra-goto-line (goto-map ""
				    :pre (display-line-numbers-mode 1)
				    :post (display-line-numbers-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(use-package lacarte
  :straight t
  :bind (("<f10>" . lacarte-execute-menu-command)))

(use-package windmove
  :straight t
  :init (windmove-default-keybindings))
(use-package framemove
  :straight t
  :init (setq framemove-hook-into-windmove t))
(global-set-key "\M-o" 'other-window)

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(use-package rg
  :straight t
  :init (rg-enable-default-bindings))

(use-package phi-search
  :straight t
  :diminish phi-search
  :config
  (progn
    (global-set-key (kbd "C-s") 'phi-search)
    (global-set-key (kbd "C-r") 'phi-search-backward)))

(use-package yasnippet
  :straight t
  :init (yas-global-mode 1))

(use-package which-key
  :straight t
  :init (which-key-mode))

(use-package aggressive-indent
  :straight t)

(use-package multiple-cursors
  :straight t
  :init (defhydra multiple-cursors-hydra (global-map "C-x m")
	  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regep
^ ^             ^ ^             [_i_] Insert numbers
^ ^             ^ ^             [_h_] Hide unmatched
^ ^             ^ ^             [_s_] Sort regions
^ ^             ^ ^             [_q_] Quit
"
	  ("i" mc/insert-numbers)
	  ("h" mc-hide-unmatched-lines-mode)
	  ("s" mc/sort-regions)
	  ("l" mc/edit-lines :exit t)
	  ("a" mc/mark-all-like-this :exit t)
	  ("n" mc/mark-next-like-this)
	  ("N" mc/skip-to-next-like-this)
	  ("M-n" mc/unmark-next-like-this)
	  ("p" mc/mark-previous-like-this)
	  ("P" mc/skip-to-previous-like-this)
	  ("M-p" mc/unmark-previous-like-this)
	  ("r" mc/mark-all-in-region-regexp :exit t)
	  ("q" nil))
  :bind (("C-^" . set-rectangular-region-anchor)
	 ("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/mark-previous-like-this)
	 ("M-#" . mc/unmark-next-like-this)
	 ("M-$" . mc/unmark-previous-like-this)))

(use-package expand-region
  :straight t
  :defer t
  :bind (("M-2" . er/expand-region)))

(use-package company
  :straight t
  :diminish
  :config (global-company-mode))

(defun my/forward-transpose-whitespace (begin end)
  "If mark is active, swap leading whitespace with region between
      point and mark. If mark isn't active, find the first
      non-whitespace character after point and swap it with the
      whitespace before it. To start, place point on character or at
      start of region."
  (interactive "*r")
  (let* ((string-to-be-switched
          (if (use-region-p)
              (delete-and-extract-region begin end)
            (progn
              (skip-chars-forward "[:space:]")
              (delete-and-extract-region (point) (1+ (point))))))
         (right-anchor (point))
         (whitespace
          (progn
            (skip-chars-backward "[:space:]")
            (delete-and-extract-region (point) right-anchor))))
    (insert string-to-be-switched whitespace)))

(global-set-key (kbd "C-c t") 'my/forward-transpose-whitespace)

(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "C-x M-q" 'my/unfill-paragraph)

(defun my/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(bind-key "M-q" 'my/fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(bind-key "M-SPC" 'cycle-spacing)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package ws-butler
  :straight t
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

(setq require-final-newline 't)

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package lsp-mode
  :straight t
  :diminish
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp)
	 (python-mode . lsp)
	 (c-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package tree-sitter
  :straight t
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :straight t)

(setq org-src-window-setup 'current-window)

(defun my/copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (file-name-base))
        (mode (symbol-name major-mode))
        (contents
         (if (use-region-p) (buffer-substring beg end) (buffer-string)))
        (gist (if (use-region-p) (gist-region beg end) (gist-buffer))))
    (kill-new
     (format "\n[[%s][Gist: %s]]\n#+begin_src %s\n%s\n#+end_src\n"
             (oref (oref gist :data) :html-url) filename
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))

(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

(use-package compile
  :straight t
  :init (progn
	  (add-hook 'c-mode-common-hook (lambda () (local-set-key "\C-c\C-m" 'compile)))
	  (add-hook 'fortran-mode-hook (lambda () (local-set-key "\C-c\C-m" 'compile)))
	  (add-hook 'f90-mode-hook (lambda () (local-set-key "\C-c\C-m" 'compile)))
	  (add-hook 'makefile-gmake-mode-hook (lambda () (local-set-key "\C-c\C-m" 'compile)))
	  (add-hook 'compilation-mode-hook (lambda () (local-set-key "\C-c\C-m" 'compile))))
  (setq compilation-scroll-output 'first-error))

(defhydra hydra-next-error
  (global-map "C-x")
  "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
  ("`" next-error     nil)
  ("j" next-error     nil :bind nil)
  ("k" previous-error nil :bind nil)
  ("h" first-error    nil :bind nil)
  ("l" (condition-case err
           (while t
             (next-error))
         (user-error nil))
   nil :bind nil)
  ("q" nil            nil :color blue))

(use-package auctex
  :straight t
  :defer t
  :config (progn (setq TeX-PDF-mode t)
		 (add-hook 'LaTeX-mode-hook '(lambda () (flyspell-mode 1)))))

(use-package slime
  :straight t
  :config (setq slime-contribs '(slime-fancy)
		inferior-lisp-program "/usr/bin/sbcl"))

(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-e") 'my/eval-and-replace)

(setq python-python-command "python3.10")
(setq python-shell-interpreter "python3.10")

(use-package lsp-pyright
  :straight t
  :after lsp-mode
  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off"))

(use-package rustic
  :straight t)

(setq fortran-comment-region "!"
      fortran-line-length 200)

(use-package pandoc-mode
  :straight t)
(use-package markdown-mode
  :straight t
  :init (progn
	  (add-hook 'markdown-mode-hook 'pandoc-mode)))

(setq c-default-style "bsd"
      c-basic-offset 2
      indent-tabs-mode nil)

(c-set-offset 'cpp-macro 0 nil)

(add-hook 'c++-mode-hook '(lambda ()
                            (define-key c++-mode-map "\C-cf" 'align-current)))

(add-hook 'c-mode-hook '(lambda ()
                          (define-key c-mode-map "\C-cf" 'align-current)))

(add-hook 'c++-mode-hook '(lambda ()
                            (key-chord-define c++-mode-map ";;" "\C-e;")))

(add-hook 'c-mode-hook '(lambda ()
                          (key-chord-define c++-mode-map ";;" "\C-e;")))

(use-package cuda-mode
  :straight t
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))))

(use-package plantuml-mode
  :straight t
  :init (progn
	  (setq plantuml-executable-path "/usr/bin/plantuml")
	  (setq plantuml-default-exec-mode 'executable)))

(use-package lua-mode
  :straight t
  :config (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))

(provide 'dot-emacs)
;;; dot-emacs ends here
