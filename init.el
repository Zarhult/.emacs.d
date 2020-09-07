;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Don't automatically refresh package contents (note: fresh install may need to manually do so)
;; package.el has to be loaded first
(require 'package)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set up package.el to work with MELPA and org mode
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org"   . "https://orgmode.org/elpa/"))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always install use-package packages if not already installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; 12-pt font
(set-face-attribute 'default nil :height 120)

;; Delete current theme before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Load theme
(use-package xresources-theme
  :config
  (load-theme 'xresources t))
;(use-package base16-theme
;  :config
;  (load-theme 'base16-atelier-lakeside t))
;(use-package jazz-themep
;  :config
;  (load-theme 'jazz t))
;(use-package gruvbox-theme
;  :config
;  (load-theme 'gruvbox-dark-hard t))
;(use-package kaolin-themes
;  :config
;  (load-theme 'kaolin-dark t))

;; Set Japanese font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "IPAMincho")))

;; Keep custom-set-variables/custom-set-faces in a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Cleaner ui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Show line/column in statusbar
(setq column-number-mode t)

;; Visual line mode, word wrap
(global-visual-line-mode t)

;; Highlight matching parens
(show-paren-mode t)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Tabs are 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")

;; Don't clutter filesystem
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hide dotfiles in dired
(require 'dired)
(setq dired-listing-switches "-lhF")

;; Functions to toggle showing dotfiles with C-.
(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer"
  (let* ((dir (dired-current-directory)))
    (progn (kill-buffer (current-buffer))
           (dired dir))))
(defun toggle-dired-listing-switches ()
  "Toggle dired listing -A flag and refresh, for showing/hiding dotfiles"
  (interactive)
  (progn
    (if (string-match "[Aa]" dired-listing-switches)
        (setq dired-listing-switches "-lhF")
      (setq dired-listing-switches "-lhFA"))
    (reload-current-dired-buffer)))
(define-key dired-mode-map (kbd "C-.") 'toggle-dired-listing-switches)

;; Launch from xdg-open in dired
(defun dired-open-file ()
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(define-key dired-mode-map (kbd "C-c o") 'dired-open-file)

;; Quick launch dired on music directory
(global-set-key (kbd "C-c C-m") (lambda () (interactive) (dired "~/音楽/")))

;; Use pdflatex
(setq latex-run-command "pdflatex")

;; Development packages
(setq lsp-keymap-prefix "C-c l")
;; Note that must install language servers (ccls, pip install ‘python-language-server[all]’)
(use-package lsp-mode
  :hook (((c-mode c++-mode objc-mode) . lsp)
         (python-mode . lsp))
  :commands lsp
  :config
  (setq lsp-completion-provider :capf)
  ;; Add pip install directory to PATH
  (setenv "PATH" (concat (getenv "PATH") "~/.local/bin/"))
  (setq exec-path (append exec-path '("~/.local/bin"))))
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after lsp-mode treemacs
  :config
  (lsp-treemacs-sync-mode 1))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;(use-package lsp-ui :commands lsp-ui-mode)
(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))
(use-package flycheck)
(use-package ccls
  :config
  (defun ccls-cmake ()
    "Generate compile_commands.json from a directory where CMake has been run"
    (interactive)
    (shell-command "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES")
    (shell-command "ln -sf Debug/compile_commands.json .")))

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")) ; Has to be done before loading mode
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package treemacs
  :bind ("C-c t" . treemacs)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))
(use-package treemacs-magit
  :after treemacs magit)
(require 'treemacs-icons-dired) ; Otherwise might have to run treemacs first

;; Lsp performance boost
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-idle-delay 0.5)

;; Other packages
(use-package multiple-cursors
  :bind ("C-c m" . mc/edit-lines))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-M-;" . avy-goto-line)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package counsel
  :bind (("C-s" . swiper-isearch)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f2> j" . counsel-set-variable)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package emms
  :bind (("C-c e p" . emms-pause)
         ("C-c e f" . emms-next)
         ("C-c e b" . emms-previous)
         ("C-c e s" . emms-stop)
         ("C-c e <right>" . (lambda () (interactive) (emms-seek 20)))
         ("C-c e <left>" . (lambda () (interactive) (emms-seek -20)))
         :map dired-mode-map
         ("C-c e SPC" . emms-play-dired))
  :config
  (require 'emms-setup)
  (require 'emms-player-mpv)

  (emms-all)
  (setq emms-player-list '(emms-player-mpv)))

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
