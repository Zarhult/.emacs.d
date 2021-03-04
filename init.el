;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Increase garbage collection during startup, for faster launch
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;; Set up use-package
;; Ensure package.el is loaded
(require 'package)

;; Set up package.el to work with MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; For fresh install, install use-package, and refresh package contents
(unless (package-installed-p 'use-package)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;; Always install use-package packages if not already installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always defer package loading unless specified otherwise with :demand t
(setq use-package-always-defer t)

;; Automatically update and remove old packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;; Themes
;; Delete current theme before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes)
(use-package cherry-blossom-theme)

(defvar dark-theme 'my-manoj-dark) ; Default dark theme
(defvar light-theme 'doom-one-light) ; Default light theme

;; Function and keybind to toggle between dark and light theme
(defun toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) dark-theme)
      (load-theme light-theme t)
    (load-theme dark-theme t)))
(global-set-key (kbd "C-c k") 'toggle-theme)

;; Load dark theme by default
(load-theme dark-theme t)

;;; General configuration
;; Highlight matching parens
(show-paren-mode t)

;; Show column in modeline
(column-number-mode t)

;; Font size
(set-face-attribute 'default nil :height 120)

;; Set Japanese font (if not in terminal emacs)
(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family "IPAGothic"))))

;; Enable visual line mode, mainly to hide arrow icons
(global-visual-line-mode t)

;; Keep custom-set-variables/custom-set-faces in a separate file, creating it if necessary
(unless (file-exists-p "~/.emacs.d/custom.el")
  (with-temp-buffer (write-file "~/.emacs.d/custom.el")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Don't blink cursor
(blink-cursor-mode -1)

;; Show time in modeline, without load average
;; Note that must enable the mode only after the configuration
(setq display-time-default-load-average nil)
(display-time-mode t)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll line by line
(setq scroll-conservatively 10000)

;; Tabs are 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")

;; Don't clutter filesystem with backups (store them in /tmp/)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Prettify symbols
(global-prettify-symbols-mode t)

;; Don't auto-scale images
(setq-default image-auto-resize 1)
;; Keybinds for ease of horizontal scrolling of images
(require 'image-mode)
(define-key image-mode-map (kbd "<tab>") 'image-scroll-left)
(define-key image-mode-map (kbd "<backtab>") 'image-scroll-right)

;; Use pdflatex
(setq latex-run-command "pdflatex")

;; gdb/gud configuration (show source code alongside gud buffer by default)
(setq gdb-show-main t)

;; Cache passwords for 5 minutes
(setq password-cache t)
(setq password-cache-expiry 300)

;;; Non-package-related keybindings, and related configuration
;; Automatically move cursor into newly created windows
(defun split-and-follow-horizontally ()
  "Split horizontally and move the cursor into the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Split vertically and move the cursor into the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Hide dotfiles in dired
(require 'dired)
(setq dired-listing-switches "-lhF")

;; Functions and keybind to toggle showing dotfiles with C-.
(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer"
  (let* ((dir (dired-current-directory)))
    (progn (kill-buffer (current-buffer))
           (dired dir))))
(defun toggle-dired-listing-switches ()
  "Toggle dired listing -A flag and reload the buffer, for showing/hiding dotfiles"
  (interactive)
  (progn
    (if (string-match "[Aa]" dired-listing-switches)
        (setq dired-listing-switches "-lhF")
      (setq dired-listing-switches "-lhFA"))
    (reload-current-dired-buffer)))
(define-key dired-mode-map (kbd "C-.") 'toggle-dired-listing-switches)

;; Launch from xdg-open in dired
(defun dired-open-file ()
  "Open a file in dired using xdg-open."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(define-key dired-mode-map (kbd "C-c o") 'dired-open-file)

;; Launch image-dired buffer for thumbnails of all images in current directory
(define-key dired-mode-map (kbd "C-t k") (lambda () (interactive) (image-dired ".")))
(setq image-dired-show-all-from-dir-max-files 999)

;; Function and keybind to launch eshell in a new window
(defun eshell-other-window ()
  "Launch eshell in another window, or switch to eshell buffer in another window if buffer exists."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))
(global-set-key (kbd "C-c s") 'eshell-other-window)

;; Function and keybind to launch dired on music directory in another window
(defun dired-music-other-window ()
  "Launch dired on ~/音楽/ in another window, or switch to that buffer in another window if it already exists."
  (interactive)
  (if (not (get-buffer "音楽"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (dired "~/音楽/"))
    (switch-to-buffer-other-window "音楽")))
(global-set-key (kbd "C-c m") 'dired-music-other-window)

;; Keybinds for setting/pausing/ending a timer
(require 'org) ; Otherwise these functions may not be available
(global-set-key (kbd "C-c j s") 'org-timer-set-timer)
(global-set-key (kbd "C-c j p") 'org-timer-pause-or-continue)
(global-set-key (kbd "C-c j k") 'org-timer-stop)

;; Make C-x k simply kill the current buffer without a prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Packages - general
(use-package diminish
  :demand t ; Otherwise won't automatically diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode))

(use-package ivy
  :diminish
  :demand t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package avy
  :bind (("C-;" . avy-goto-word-or-subword-1)
         ("C-M-;" . avy-goto-line)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :diminish
  :config
  (which-key-mode t))

(use-package emms
  :bind (("C-c e p" . emms-pause)
         ("C-c e f" . emms-next)
         ("C-c e b" . emms-previous)
         ("C-c e s" . emms-stop)
         ("C-c e <right>" . (lambda () (interactive) (emms-seek 20)))
         ("C-c e <left>" . (lambda () (interactive) (emms-seek -20)))
         :map dired-mode-map
         ("C-c e <SPC>" . emms-play-dired))
  :config
  (require 'emms-setup)
  (require 'emms-player-mpv)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package pdf-tools ; Must install app-text/poppler with "cairo" use flag on gentoo
  :demand t ; Otherwise emacs will default to DocView
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (pdf-tools-install))

;;; Packages - programming
(use-package projectile
  :diminish
  :init
  (projectile-mode t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  ;; Ignore "build" directories
  (setq projectile-indexing-method 'native)
  (push '"build" projectile-globally-ignored-directories)
  (push '"CMakeFiles" projectile-globally-ignored-directories)
  (push '"Debug" projectile-globally-ignored-directories)  
  ;; Automatically parse all projects in ~/dev
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev"))
    (projectile-discover-projects-in-search-path)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode c++-mode objc-mode) . lsp) ; Must have ccls, clangd, or similar installed
         (html-mode . lsp) ; To install server: "npm install -g vscode-html-languageserver-bin"
         (css-mode . lsp) ; To install server: "npm install -g vscode-css-languageserver-bin"
         (js-mode . lsp) ; To install server: "npm install -g typescript-language-server; npm install -g typescript"
         (python-mode . lsp) ; To install server: "pip install 'python-language-server[all]'"
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; Override default lsp M-n/M-p bindings
  (require 'flymake)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  ;; Disable some functionality
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable nil) ; Hide headerline
  ;; lsp-mode performance boosting
  ;; Be sure to use emacs version 27+ compiled with native json support
  (setq gc-cons-threshold 6400000)
  (setq read-process-output-max (* 1024 1024)))
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
(use-package lsp-ivy
  :demand t ; Necessary for remap to work
  :after lsp-mode ivy
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol))

(use-package company
  :diminish)
(use-package yasnippet
  :config
  (yas-global-mode t)) ; Necessary for html completion

(use-package magit
  :bind ("C-c g" . magit-file-dispatch))
