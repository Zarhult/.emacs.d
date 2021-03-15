;; Increase garbage collection and temporarily unset file-name-handler-alist
;; during startup, for faster launch
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq startup-gc-cons-threshold gc-cons-threshold)
(setq startup-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun startup-reset ()
  (setq gc-cons-threshold startup-gc-cons-threshold
        gc-cons-percentage startup-gc-cons-percentage
        file-name-handler-alist last-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'startup-reset)

;;; Set up package installation necessities
;; First set up package.el with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; If auto-package-update is not installed, treat this as a fresh install
;; and refresh package contents before continuing
(unless (package-installed-p 'auto-package-update)
  (package-refresh-contents))

(defun install-if-not-installed (package)
  "Checks if given package is installed and installs it if not."
  (unless (package-installed-p package)
    (package-install package)))

;; Automatically update packages and remove old packages
(install-if-not-installed 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(setq auto-package-update-hide-results t)
(auto-package-update-maybe)

;;; Themes
;; Delete current theme before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(install-if-not-installed 'cherry-blossom-theme)
(install-if-not-installed 'gruvbox-theme)

(defvar my-dark-theme 'cherry-blossom) ; Default dark theme
(defvar my-light-theme 'leuven) ; Default light theme

;; Function and keybind to toggle between dark and light theme
(defun toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) my-dark-theme)
      (load-theme my-light-theme t)
    (load-theme my-dark-theme t)))
(define-key global-map (kbd "C-c k") 'toggle-theme)

;; Load dark theme by default
(load-theme my-dark-theme t)

;;; Basic configuration
;; Enable C-x C-l, C-x C-u
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; Keep custom-set-variables/custom-set-faces in a separate file,
;; creating it if necessary
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
(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "<tab>") 'image-scroll-left)
  (define-key image-mode-map (kbd "<backtab>") 'image-scroll-right))

;; Use pdflatex
(setq latex-run-command "pdflatex")

;; gdb/gud configuration (show source code alongside gud buffer by default)
(setq gdb-show-main t)

;; Cache passwords for 5 minutes
(setq password-cache t)
(setq password-cache-expiry 300)

;; Fuzzy-matching, etc
(setq completion-styles '(initials partial-completion flex))
(setq completion-cycle-threshold 10)

;; Make .conf files and extensionless files trigger text-mode
(add-to-list 'auto-mode-alist '("\\.conf\\'" . text-mode))
(add-to-list 'auto-mode-alist '("/[^\\./]*\\'" . text-mode))

;; Relative line numbers, when text editing only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;; My main functions
(defun split-and-follow-horizontally ()
  "Split horizontally and move the cursor into the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun split-and-follow-vertically ()
  "Split vertically and move the cursor into the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer"
  (let ((dir (dired-current-directory)))
    (progn (kill-buffer (current-buffer))
           (dired dir))))
(defun toggle-dired-listing-switches ()
  "Toggle dired listing -A flag and reload the buffer.
Effectively toggles showing/hiding dotfiles."
  (interactive)
  (progn
    (if (string-match "[Aa]" dired-listing-switches)
        (setq dired-listing-switches "-lhF")
      (setq dired-listing-switches "-lhFA"))
    (reload-current-dired-buffer)))
;; Hide dotfiles in dired by default
(with-eval-after-load 'dired
  (setq dired-listing-switches "-lhF"))

(defun dired-xdg-open ()
  "Open a file in dired using xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun eshell-other-window ()
  "Launch eshell in another window, or switch to eshell buffer in another window
 if buffer exists. If the former, launch inside the current directory."
  (interactive)
  (unless (get-buffer "*eshell*")
    (eshell)
    (previous-buffer))
  (switch-to-buffer-other-window "*eshell*"))

(defun ansi-term-other-window ()
  "Launch ansi-term with bash in another window, or switch to ansi-term buffer
in another window if buffer exists. If the former, launch inside the current
directory."
  (interactive)
  (unless (get-buffer "*ansi-term*")
    (ansi-term "/bin/bash")
    (previous-buffer))
  (switch-to-buffer-other-window "*ansi-term*"))

(defun dired-other-window-current-directory ()
  "Launch dired on the current directory in another window."
  (interactive)
  (let ((dir (file-name-nondirectory
              (directory-file-name
               (file-name-directory buffer-file-name)))))
    (unless (get-buffer dir)
      (dired ".")
      (previous-buffer))
    (split-window-sensibly (selected-window))
    (switch-to-buffer-other-window dir)))

(defun dired-music-other-window ()
  "Launch dired on ~/音楽/ in another window, or switch to that buffer in
another window if it already exists."
  (interactive)
  (if (not (get-buffer "音楽"))
      (progn
        (split-window-sensibly)
        (other-window 1)
        (dired "~/音楽/"))
    (switch-to-buffer-other-window "音楽")))

(defun image-dired-current-directory ()
  "Launch image-dired on the current directory."
  (interactive)
  (image-dired "."))

;; Better asm-mode indentation
(defun my-asm-mode-hook ()
  "Hook to override default asm-mode indentation function."
  (defun asm-calculate-indentation ()
    (or
     ;; Flush labels to the left margin.
     (and (looking-at "\\(\\.\\|\\sw\\|\\s_\\)+:") 0)
     (and (looking-at "[.@_[:word:]]+:") 0)
     ;; Same thing for `;;;' comments.
     (and (looking-at "\\s<\\s<\\s<") 0)
     ;; %if nasm macro stuff goes to the left margin
     (and (looking-at "%") 0)
     (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
     ;; The rest goes at column 4
     (or 4))))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;;; Keybinds - for my functions
(define-key global-map (kbd "C-x 2") 'split-and-follow-horizontally)
(define-key global-map (kbd "C-x 3") 'split-and-follow-vertically)
(define-key global-map (kbd "C-c s") 'eshell-other-window)
(define-key global-map (kbd "C-c a") 'ansi-term-other-window)
(define-key global-map (kbd "C-c d") 'dired-other-window-current-directory)
(define-key global-map (kbd "C-c m") 'dired-music-other-window)
(define-key global-map (kbd "C-c i") 'image-dired-current-directory)
(setq image-dired-show-all-from-dir-max-files 999)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c o") 'dired-xdg-open)
  (define-key dired-mode-map (kbd "C-.") 'toggle-dired-listing-switches))

;;; Keybinds - for unbound built-in functions
;; Make C-x k kill current buffer without prompt
(define-key global-map (kbd "C-x k") 'kill-this-buffer)
;; Autoload org-timer functions before binding them
(dolist (func '(org-timer-set-timer
                org-timer-pause-or-continue
                org-timer-stop))
  (autoload func "org-timer"))
(define-prefix-command 'org-timer-map)
(define-key global-map (kbd "C-c j") 'org-timer-map)
(define-key 'org-timer-map (kbd "s") 'org-timer-set-timer)
(define-key 'org-timer-map (kbd "p") 'org-timer-pause-or-continue)
(define-key 'org-timer-map (kbd "k") 'org-timer-stop)

;;; Packages - general
;; (install-if-not-installed 'evil)
;; (require 'evil)
;; (with-eval-after-load 'evil
;;   (setq evil-insert-state-cursor t)
;;   ;; Use evil for text editing only
;;   (setq evil-default-state 'emacs)
;;   (evil-set-initial-state 'prog-mode 'normal)
;;   (evil-set-initial-state 'text-mode 'normal)
;;   (evil-mode t))

(install-if-not-installed 'diminish)
;; First diminish built-in visual-line-mode without an eval-after-load
(diminish 'visual-line-mode)
;; Now diminish modes that need to be diminished only after they load
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode))
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode))
(with-eval-after-load 'projectile
  (diminish 'projectile-mode))

(install-if-not-installed 'avy)
(define-key global-map (kbd "C-;") 'avy-goto-char)
(define-key global-map (kbd "C-M-;") 'avy-goto-line)

(install-if-not-installed 'expand-region)
(define-key global-map (kbd "C-=") 'er/expand-region)

(install-if-not-installed 'which-key)
(which-key-mode t)

(install-if-not-installed 'emms)
(define-prefix-command 'emms-map)
(define-key global-map (kbd "C-c e") 'emms-map)
(define-key 'emms-map (kbd "p") 'emms-pause)
(define-key 'emms-map (kbd "f") 'emms-next)
(define-key 'emms-map (kbd "b") 'emms-previous)
(define-key 'emms-map (kbd "s") 'emms-stop)
(define-key 'emms-map (kbd "<right>") 'emms-seek-forward)
(define-key 'emms-map (kbd "<left>") 'emms-seek-backward)
(define-key 'emms-map (kbd "SPC") 'emms-play-dired)
(emms-all)
(setq emms-player-list '(emms-player-mpv))

(install-if-not-installed 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Note for pdf-tools, must install app-text/poppler
;; with "cairo" use flag on gentoo
(install-if-not-installed 'pdf-tools)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
(pdf-tools-install)

;;; Packages - programming
(install-if-not-installed 'projectile)
(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; Ignore directories commonly associated with building
(setq projectile-indexing-method 'native)
(push '"build" projectile-globally-ignored-directories)
(push '"CMakeFiles" projectile-globally-ignored-directories)
(push '"Debug" projectile-globally-ignored-directories)
;; Automatically parse all projects in ~/dev
(when (file-directory-p "~/dev")
  (setq projectile-project-search-path '("~/dev"))
  (projectile-discover-projects-in-search-path))

(install-if-not-installed 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")
;; Hook into lsp-mode for each mode hook listed below
;; Language servers must be installed externally for lsp to work
;; C/C++: Must have ccls, clangd, or similar installed
;; html: "npm install -g vscode-html-languageserver-bin"
;; css: "npm install -g vscode-css-languageserver-bin"
;; js: "npm install -g typescript-language-server; npm install -g typescript"
;; python: "pip install 'python-language-server[all]'"
(defvar lsp-mode-hooks '(c-mode-hook c++-mode-hook objc-mode-hook
                         html-mode-hook css-mode-hook js-mode-hook
                         python-mode-hook))
(dolist (hook lsp-mode-hooks)
  (add-hook hook #'lsp-deferred))
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;; Disable some functionality
(setq lsp-enable-on-type-formatting nil)
(setq lsp-headerline-breadcrumb-enable nil) ; Hide headerline
;; Enable stricter linting for css
(setq lsp-css-lint-duplicate-properties t
      lsp-css-lint-zero-units t
      lsp-css-lint-universal-selector t
      lsp-css-lint-box-model t
      lsp-css-lint-important t
      lsp-css-lint-float t
      lsp-css-lint-id-selector t)
;; lsp-mode performance boosting
;; Be sure to use emacs version 27+ compiled with native json support
(setq gc-cons-threshold 6400000)
(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(install-if-not-installed 'lsp-ui)
(with-eval-after-load 'lsp-ui
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions]
    'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]
    'lsp-ui-peek-find-references))

(install-if-not-installed 'company)

(install-if-not-installed 'yasnippet)
(yas-global-mode t) ; Need yasnippet for html completion

(install-if-not-installed 'magit)
(define-key global-map (kbd "C-c g") 'magit-file-dispatch)

