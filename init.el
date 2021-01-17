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

;; Set startup image
(setq fancy-splash-image (expand-file-name "images/lain.png" user-emacs-directory))

;; Delete current theme before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Load theme
(use-package xresources-theme
  :config
  (load-theme 'xresources t)
  ;; Use highlight instead of bar for hl-face colors
  ;  (require 'hl-line)
  ;  (set-face-background 'hl-line (face-attribute 'default :foreground))
  ;  (set-face-foreground 'hl-line (face-attribute 'default :background)
  ;; Make paren highlighting match theme
  (set-face-background 'show-paren-match (face-attribute 'success :foreground)))

;; Set Japanese font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "IPAMincho")))

;; Enable visual line mode, mainly to hide arrow icons
(global-visual-line-mode 1)

;; Keep custom-set-variables/custom-set-faces in a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Cleaner ui
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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

;; Don't auto-scale images
(setq image-transform-resize 1)

;; Use pdflatex
(setq latex-run-command "pdflatex")

;; Winner mode
(winner-mode 1)

;; Cache passwords for 5 minutes, do so in eshell with tramp
(setq password-cache t)
(setq password-cache-expiry 300)
(require 'em-tramp)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

;; Packages
(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package multiple-cursors
  :bind ("C-c m" . mc/edit-lines))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-M-;" . avy-goto-line)))

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

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("v" . elfeed-view-mpv))
  :config
  (defun elfeed-v-mpv (url)
    "Watch a video from URL in MPV"
    (async-shell-command (format "mpv %s" url)))
  
  (defun elfeed-view-mpv (&optional use-generic-p)
    "Play youtube video from feed"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-v-mpv it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line)))))

(use-package eyebrowse
  :config
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Programming stuff/packages
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(setq lsp-keymap-prefix "C-c l") ; This line can't go inside the lsp-mode :config section
(use-package lsp-mode
  :hook (((c-mode c++-mode objc-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-keymap-prefix "C-c l"))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (defun ccls-cmake ()
    "Generate compile_commands.json from a directory where CMake has been run"
    (interactive)
    (shell-command "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES")
    (shell-command "ln -sf Debug/compile_commands.json .")))

(use-package magit
  :config
  :bind ("C-c g" . magit-file-dispatch))

;; Better asm-mode indentation
(defun my-asm-mode-hook ()
;  (electric-indent-local-mode)  ; toggle off
;  (setq tab-width 4)
;  (setq indent-tabs-mode nil)
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  ;; (setq tab-always-indent (default-value 'tab-always-indent))
  
  (defun asm-calculate-indentation ()
    (or
     ;; Flush labels to the left margin.
 ;   (and (looking-at "\\(\\.\\|\\sw\\|\\s_\\)+:") 0)
     (and (looking-at "[.@_[:word:]]+:") 0)
     ;; Same thing for `;;;' comments.
     (and (looking-at "\\s<\\s<\\s<") 0)
     ;; %if nasm macro stuff goes to the left margin
     (and (looking-at "%") 0)
     (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
     ;; Simple `;' comments go to the comment-column
     ;(and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     ;; The rest goes at column 4
     (or 4))))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)
