(package-initialize)
(add-to-list 'load-path "~/.emacs.d/site-packages/")
(setq package-enable-at-startup nil)

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

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package hydra)

(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package key-chord
  :init (key-chord-mode 1))

(use-package dash)

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

(use-package ace-window)
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
     ("b" helm-mini)
     ("f" helm-find-files)
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

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (require 'helm-adaptive)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c C-h"     . helm-mini)
         ("C-h a"     . helm-apropos)
         ("C-x C-b"   . helm-buffers-list)
         ("C-x b"     . helm-buffers-list)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         ("C-x c o"   . helm-occur)
         ("C-x c s"   . helm-swoop)
         ("C-c h m"   . helm-man-woman)
         ("C-c h f"   . helm-find)
         ("C-c h l"   . helm-locate)
         ("C-c h r"   . helm-resume)
         ("C-h r"     . helm-info-emacs)
         ("C-x C-f"   . helm-find-files)
         ("C-x c SPC" . helm-all-mark-rings)
         :map helm-map
         ("<tab>"     . helm-execute-persistent-action)
         ("C-i"       . helm-execute-persistent-action)
         ("C-z"       . helm-select-action)))


(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package smart-mode-line)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(use-package zenburn-theme
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

(use-package powerline
  :config (powerline-default-theme))

(use-package tramp
  :init (setq tramp-unified-filename t))

(use-package tramp-term)

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
  :init (progn (require 'pcmpl-unix) (defun my/ssh-remote-term (hostname)
  (interactive (list (completing-read "Hostname: " (pcmpl-ssh-hosts))))
  (my/remote-term hostname "ssh" hostname))))

(defun helm-source-ssh-remote-term ()
  (helm-build-sync-source "SSH hostname"
			  :candidates (lambda () (pcmpl-ssh-hosts))
			  :filtered-candidate-transformer '(helm-adaptive-sort)
			  :nomark t
			  :action '(("Select host" . my/ssh-remote-term))))

(defun my/helm-ssh-remote-term ()
  (interactive)
  (helm :sources (helm-source-ssh-remote-term)
	:buffer "*helm-ssh-remote-term*"))

(defun my/local-term ()
  (interactive)
  (ansi-term "bash" "localhost"))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(use-package ibuffer
  :bind (("<f9>" . ibuffer))
  :init (setq ibuffer-shrink-to-minimum-size t
              ibuffer-always-show-last-buffer nil
              ibuffer-sorting-mode 'recency
              ibuffer-use-header-line t))

(add-to-list 'vc-handled-backends 'GIT)

(setq vc-follow-symlinks t)

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
  :init (setq magit-auto-revert-mode t)
  :bind (("C-x C-g" . magit-status)))

(require 'tramp)
(add-to-list 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))

(use-package projectile
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
(use-package helm-projectile)

(global-set-key "\C-cg" 'goto-line)

(use-package avy
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
				    :pre (linum-mode 1)
				    :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

(use-package lacarte
 :ensure nil
 :bind (("<f10>" . lacarte-execute-menu-command)))

(use-package windmove
  :ensure nil
  :init (windmove-default-keybindings))
(use-package framemove
  :ensure nil
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

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package which-key)

(use-package aggressive-indent)

(use-package multiple-cursors
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
  :defer t
  :bind (("M-2" . er/expand-region)))

(use-package company
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

(setq require-final-newline 't)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp)
         (python-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package tree-sitter
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

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
   :defer t
   :config (progn (setq TeX-PDF-mode t)
                  (add-hook 'LaTeX-mode-hook '(lambda () (flyspell-mode 1)))))

(use-package slime
   :ensure t
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

(setq python-python-command "python3")
(setq python-shell-interpreter "python3")

(setq fortran-comment-region "!"
	  fortran-line-length 200)

(use-package pandoc-mode)
(use-package markdown-mode
  :ensure t
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

(provide 'dot-emacs)
;;; dot-emacs ends here
