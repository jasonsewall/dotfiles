(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; (defun bootstrap-straight ()
;;   "Install and configure straight.el"
;;   (setq straight-vc-git-default-clone-depth 'full
;;         straight-check-for-modifications '(check-on-save find-when-checking)
;;         straight-build-dir (format "build-%s" emacs-version))
;;   (defvar bootstrap-version)
;;   (let ((bootstrap-file
;;          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;         (bootstrap-version 5))
;;     (unless (file-exists-p bootstrap-file)
;;       (with-current-buffer
;;           (url-retrieve-synchronously
;;            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;            'silent 'inhibit-cookies)
;;         (goto-char (point-max))
;;         (eval-print-last-sexp)))
;;     (load bootstrap-file nil 'nomessage)))

;; (defun bootstrap-use-package ()
;;   "Install use-package.el"
;;   (setq use-package-enable-imenu-support t)
;;   (straight-use-package 'use-package)
;;   (use-package diminish :straight t :defer t))

;; (bootstrap-straight)
;; (bootstrap-use-package)

(provide 'early-init)
