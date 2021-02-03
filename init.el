;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Ensure package.el is loaded
(require 'package)

;; Set up package.el to work with MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; For first startup, install use-package, and refresh package contents
(unless (package-installed-p 'use-package)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

;; Always install use-package packages if not already installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically update and remove old packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Highlight matching parens
(show-paren-mode t)

;; Show column in mode-line
(setq column-number-mode t)

;; 12-pt font
(set-face-attribute 'default nil :height 120)

;; Set Japanese font (if not in terminal emacs)
(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family "IPAMincho"))))

;; Enable visual line mode, mainly to hide arrow icons
(global-visual-line-mode 1)

;; Keep custom-set-variables/custom-set-faces in a separate file, creating it if necessary
(unless (file-exists-p "~/.emacs.d/custom.el")
  (with-temp-buffer (write-file "~/.emacs.d/custom.el")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Cleaner ui
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll line by line
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

;; Prettify symbols
(global-prettify-symbols-mode t)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Don't auto-scale images
(setq image-transform-resize 1)

;; Use pdflatex
(setq latex-run-command "pdflatex")

;; gdb/gud configuration
(setq gdb-show-main t)
(setq gdb-show-main t)

;; flymake error navigation
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

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

;; Cache passwords for 5 minutes, do so in eshell with tramp
(setq password-cache t)
(setq password-cache-expiry 300)
(require 'em-tramp)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

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

;; Easier window resizing
(global-set-key (kbd "C-M-b") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-f") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-n") 'shrink-window)
(global-set-key (kbd "C-M-p") 'enlarge-window)

;; Keybind to transpose regions
(global-set-key (kbd "C-c t") 'transpose-regions)

;; Packages
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package which-key
  :config
  (which-key-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Programming stuff/packages
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev"))))

(setq lsp-keymap-prefix "C-c l") ; This line can't go inside the lsp-mode :config section
(use-package lsp-mode
  :hook (((c-mode c++-mode objc-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package ccls
  :config
  (defun ccls-cmake ()
    "Generate compile_commands.json from a directory where CMake has been run."
    (interactive)
    (shell-command "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES")
    (shell-command "ln -sf Debug/compile_commands.json .")))

(use-package magit
  :bind ("C-c g" . magit-file-dispatch))

;; Better asm-mode indentation
(defun my-asm-mode-hook ()
  (defun asm-calculate-indentation ()
    (or
     (and (looking-at "[.@_[:word:]]+:") 0)
     (and (looking-at "\\s<\\s<\\s<") 0)
     (and (looking-at "%") 0)
     (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
     (or 4))))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; Delete current theme before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Finally load theme
(use-package ewal)
(use-package ewal-spacemacs-themes
  :bind ("C-c w" . (lambda () (interactive) (load-theme 'ewal-spacemacs-modern t))) ; Theme reloading keybind
  :init
  (setq spacemacs-theme-underline-parens t)
  :config
  (load-theme 'ewal-spacemacs-modern t))

;; (use-package cherry-blossom-theme
;;   :config
;;   (load-theme 'cherry-blossom t))
