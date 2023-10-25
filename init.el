;;; First configure immediate necessities
;; Increase garbage collection and temporarily unset file-name-handler-alist
;; during startup, for faster launch
(setq startup-file-name-handler-alist file-name-handler-alist
      startup-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun startup-reset ()
  (setq gc-cons-threshold 6400000
        gc-cons-percentage startup-gc-cons-percentage
        file-name-handler-alist startup-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'startup-reset)

;; Emacs versions before 27 do not load early-init.el or
;; automatically package-initialize
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el"))
  (package-initialize))

;; Some combinations of GNU TLS and Emacs fail to retrieve
;; archive contents over https
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; Set up package installation necessities
;; First set up package.el with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; If auto-package-update is not installed, consider this a fresh install and
;; refresh package contents before continuing
(unless (package-installed-p 'auto-package-update)
  (package-refresh-contents))

(defun install-if-not-installed (package)
  "Check if PACKAGE is installed and install it if not."
  (unless (package-installed-p package)
    (package-install package)))

;; Automatically update packages and remove old packages
(install-if-not-installed 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(setq auto-package-update-hide-results t)
(auto-package-update-maybe)

;;; Theming
(defun disable-all-themes ()
  "Disable all active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(defun load-theme-from-scratch ()
  "Disable all themes and then call load-theme interactively."
  (interactive)
  (disable-all-themes)
  (call-interactively 'load-theme))

;; No bold text
;; (defun remap-faces-default-attributes ()
;;   (let ((family (face-attribute 'default :family))
;;         (height (face-attribute 'default :height)))
;;     (mapcar (lambda (face)
;;               (face-remap-add-relative
;;                face :family family :weight 'normal :height height))
;;             (face-list))))
;; (when (display-graphic-p)
;;  (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
;;  (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))

;; Install some cool themes
(install-if-not-installed 'ewal)
(dolist (theme '(base16-theme
                 doom-themes
                 cherry-blossom-theme
                 minsk-theme))
  (install-if-not-installed theme))
(setq ewal-shade-percent-difference 15) ; This makes avy's colors readable with ewal theme
;(setq doom-themes-enable-bold t)
;(setq doom-themes-enable-italic t)

;; List of good themes to cycle through, with first theme being default theme
(setq main-themes
      (list 'doom-vibrant
            'doom-one
            'doom-ayu-dark
            'doom-ayu-mirage
            'doom-city-lights
            'doom-dark+
            'doom-xcode
            'my-manoj-dark
            'cherry-blossom
            'base16-ashes
            'base16-darkviolet
            'minsk))
(setq current-theme-num 0)
(defun cycle-theme ()
  "Cycle through theme list `main-themes', and default theme."
  (interactive)
    (if (= (length main-themes) current-theme-num)
        (progn
          (disable-all-themes)
          (setq current-theme-num 0))
      (progn
        (disable-all-themes)
        (load-theme (nth current-theme-num main-themes) t)
        (setq current-theme-num (+ current-theme-num 1)))))

;; (defun reload-ewal-theme ()
;;   "Reloads the ewal-doom-one theme."
;;   (interactive)
;;   (disable-all-themes)
;;   (load-theme 'ewal-doom-one))

;; Set first theme
(cycle-theme)

;;; Basic configuration
(set-language-environment "Japanese")

;; Enable C-x C-l, C-x C-u
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Highlight matching parens
(show-paren-mode t)

;; Show column in modeline
(column-number-mode t)

;; Font and font size
(when (member "Liberation Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Liberation Mono"))
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
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Don't blink cursor, but if something starts blinking the cursor then blink indefinitely
(blink-cursor-mode -1)
(setq blink-cursor-blinks 0)

;; Show time in modeline, without load average
;; Note that must enable the mode only after the configuration
(setq display-time-default-load-average nil)
(display-time-mode t)

;; Tabs are 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Start at scratch buffer
(setq inhibit-startup-screen t)

;; Don't clutter filesystem with backups (store them in /tmp/)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Prettify symbols
(global-prettify-symbols-mode t)

;; Don't auto-fit images
;(setq-default image-auto-resize 1.2)
;; Hide mode-line when viewing images
;(add-hook 'image-mode-hook (lambda () (setq mode-line-format nil)))
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

;; Make .conf files and extensionless files trigger text-mode
(add-to-list 'auto-mode-alist '("\\.conf\\'" . text-mode))
(add-to-list 'auto-mode-alist '("/[^\\./]*\\'" . text-mode))

;; Use relative line numbers for text editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; No warnings when showing lots of images with image-dired-show-all-from-dir
(setq image-dired-show-all-from-dir-max-files 999)

;; Use Emacs 29 built-in completion with C-M-i and M-/ if using Emacs 29
(unless (version< emacs-version "29")
  (setq completions-format 'one-column)
  (setq completions-header-format nil)
  (setq completions-max-height 20)
  (setq completion-auto-select t))

;; Use eglot built in to Emacs 29 for lsp. Install it if not using Emacs 29.
(when (version< emacs-version "29")
  (install-if-not-installed 'eglot))
(setq eglot-hooks '(c-mode-hook c++-mode-hook objc-mode-hook
                                html-mode-hook css-mode-hook js-mode-hook
                                python-mode-hook))
(dolist (hook eglot-hooks)
  (add-hook hook 'eglot-ensure))
;; Disable documentation on hover/symbol highlighting
;(setq eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))

;; Flymake keybinds
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; Put flymake on right fringe so we can use git-gutter on the left fringe
(setq flymake-fringe-indicator-position 'right-fringe)

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
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(with-eval-after-load 'dired
  (setq dired-listing-switches "-lhF"))

(defun dired-xdg-open ()
  "Open a file in dired using xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun image-dired-current-directory ()
  "Launch `image-dired' on the current directory."
  (interactive)
  (image-dired "."))

(defvar mode-line-format-visible mode-line-format
  "Value of variable `mode-line-format' prior to most recent hiding of the
mode-line by `toggle-mode-line'.")
(defun toggle-mode-line ()
  "Toggle visibility of the mode-line."
  (interactive)
  (if mode-line-format
      (setq mode-line-format-visible mode-line-format
            mode-line-format nil)
    (setq mode-line-format mode-line-format-visible)
    (previous-buffer) ; Hacky way of forcing mode-line to update
    (next-buffer)))

(defun center-and-square-frame ()
  "Resize the frame to 1100 x 1000 px and then center it. Assumes 1920x1080 px monitor."
  (interactive)
  (set-frame-size (selected-frame) 1100 1000 t)
  (set-frame-position (selected-frame) 410 40))

;;; Keybinds - for my functions
(define-key global-map (kbd "C-x 2") 'split-and-follow-horizontally)
(define-key global-map (kbd "C-x 3") 'split-and-follow-vertically)
(define-key global-map (kbd "C-c s") 'eshell)
(define-key global-map (kbd "C-c i") 'image-dired-current-directory)
(define-key global-map (kbd "C-c ;") 'toggle-mode-line)
(define-key global-map (kbd "C-c '") 'load-theme-from-scratch)
(define-key global-map (kbd "C-c k") 'cycle-theme)
(define-key global-map (kbd "C-c m") 'center-and-square-frame)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c o") 'dired-xdg-open)
  (define-key dired-mode-map (kbd "C-.") 'toggle-dired-listing-switches))

;;; Keybinds - for unbound built-in functions
;; Make C-x k kill current buffer without prompt
(define-key global-map (kbd "C-x k") 'kill-this-buffer)
;; Next/prev buffer more easily than the default binding
(define-key global-map (kbd "C-,") 'previous-buffer)
(define-key global-map (kbd "C-.") 'next-buffer)

;; Disable electric indent in asm mode, indent to same level as prev lines
(defun newline-and-indent-same-level ()
  "Insert a newline, then indent to the same column as the current line."
  (interactive)
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (newline)
    (indent-to-column col)))
(defun my-asm-mode-hook ()
  (electric-indent-local-mode -1)
  (define-key asm-mode-map (kbd "RET") 'newline-and-indent-same-level)
  (setq indent-tabs-mode nil))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;;; Packages - general
(install-if-not-installed 'god-mode)
(setq god-mode-enable-function-key-translation nil) ; Leave function keys alone
(require 'god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(define-key god-local-mode-map (kbd "z") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)
(define-key god-local-mode-map (kbd "C-x C-1") #'delete-other-windows)
(define-key god-local-mode-map (kbd "C-x C-2") #'split-and-follow-horizontally)
(define-key god-local-mode-map (kbd "C-x C-3") #'split-and-follow-vertically)
(define-key god-local-mode-map (kbd "C-x C-0") #'delete-window)
(define-key god-local-mode-map (kbd "C-x C-o") #'other-window)
(define-key god-local-mode-map (kbd "C-x C-k") #'kill-this-buffer)
(define-key god-local-mode-map (kbd "C-c C-s") #'eshell)
(define-key god-local-mode-map (kbd "C-c C-'") #'load-theme-from-scratch)
(define-key god-local-mode-map (kbd "C-c C-k") #'cycle-theme)
;; Block cursor in god-mode, vertical line outside god-mode
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
(god-mode)

(install-if-not-installed 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(install-if-not-installed 'dirvish)
(dirvish-override-dired-mode)
(define-key god-local-mode-map (kbd "C-c C-l") 'dirvish-side)

(install-if-not-installed 'doom-modeline) ; Run M-x nerd-icons-install-fonts after install
(require 'doom-modeline)
(doom-modeline-mode 1)

(install-if-not-installed 'free-keys)

(install-if-not-installed 'writeroom-mode)
(define-key global-map (kbd "C-M-,") 'writeroom-mode)
(setq writeroom-bottom-divider-width 0) ; No line at bottom of window in writeroom mode
(setq writeroom-width 100)
(setq writeroom-restore-window-config t)
(with-eval-after-load 'writeroom-mode
  (delete 'writeroom-set-alpha writeroom-global-effects)) ; Don't modify transparency

;; Keep modeline clean with diminish
(install-if-not-installed 'diminish)
;; First diminish built-in visual-line-mode without an eval-after-load
(diminish 'visual-line-mode)
;; Now diminish modes that should be diminished only after they load
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
(with-eval-after-load 'company
  (diminish 'company-mode))
(with-eval-after-load 'git-gutter
  (diminish 'git-gutter-mode))

(install-if-not-installed 'expand-region)
(define-key global-map (kbd "C-=") 'er/expand-region)

(install-if-not-installed 'which-key)
(which-key-mode t)

(install-if-not-installed 'avy)
(define-key global-map (kbd "C-;") 'avy-goto-char)
(define-key global-map (kbd "C-M-/") 'avy-kill-region)

(install-if-not-installed 'emms)
(define-prefix-command 'emms-map)
(define-key global-map (kbd "C-c e") 'emms-map)
(define-key emms-map (kbd "p") 'emms-pause)
(define-key emms-map (kbd "f") 'emms-next)
(define-key emms-map (kbd "b") 'emms-previous)
(define-key emms-map (kbd "s") 'emms-stop)
(define-key emms-map (kbd "<right>") 'emms-seek-forward)
(define-key emms-map (kbd "<left>") 'emms-seek-backward)
(define-key emms-map (kbd "SPC") 'emms-play-dired)
(defun my-emms-setup ()
  "Set up emms for simple playing of music with mpv, showing the filename and
time position in the modeline. Do nothing if emms is already loaded."
  (unless (featurep 'emms-playing-time)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-player-simple)
    (require 'emms-player-mpv)
    (require 'emms-mode-line)
    (require 'emms-playing-time)
    (emms-mode-line 1)
    (emms-mode-line-blank)
    (emms-playing-time 1)
    (setq emms-player-list '(emms-player-mpv))))
;; Since these are all the emms functions I use, just setup emms when I try
;; to call one of them
(dolist (func '(emms-pause
                emms-next
                emms-previous
                emms-stop
                emms-seek-forward
                emms-seek-backward
                emms-play-dired))
  (advice-add func :before #'my-emms-setup))

;; Note that for pdf-tools, if on Gentoo, must install
;; app-text/poppler with "cairo" use flag
(install-if-not-installed 'pdf-tools)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode) ; Default dark mode
(pdf-loader-install)

;;; Packages - programming
(install-if-not-installed 'corfu)
(setq corfu-auto t)
(setq corfu-auto-prefix 2)
(global-corfu-mode)
(install-if-not-installed 'projectile)
;; Load and set up projectile only after either eglot is enabled or try to use a projectile keybind
(setq my-projectile-prefix (kbd "C-c p"))
(defun setup-projectile-with-prefix (prefix)
  "Unbind any previous global PREFIX binding, and load and enable projectile
mode with PREFIX bound to `projectile-command-map' within `projectile-mode-map'.
Do nothing if projectile is already loaded."
  (unless (featurep 'projectile)
    (interactive)
    (projectile-mode t)
    (define-key global-map my-projectile-prefix nil)
    (define-key projectile-mode-map my-projectile-prefix 'projectile-command-map)))
(define-key global-map my-projectile-prefix
  (lambda () (interactive)
    (setup-projectile-with-prefix my-projectile-prefix)
    ;; Re-input the prefix so the command input can smoothly continue after
    ;; projectile loads
    (setq unread-command-events
          (mapcar (lambda (e) `(t . ,e))
                  (listify-key-sequence my-projectile-prefix)))))
(dolist (hook eglot-hooks)
  (add-hook hook (lambda () (interactive)
                   (setup-projectile-with-prefix my-projectile-prefix))))
(with-eval-after-load 'projectile
  ;; Ignore directories commonly associated with building
  (setq projectile-indexing-method 'native)
  (push '"build" projectile-globally-ignored-directories)
  (push '"CMakeFiles" projectile-globally-ignored-directories)
  (push '"Debug" projectile-globally-ignored-directories)
  ;; Automatically parse all projects in ~/dev
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev"))
    (projectile-discover-projects-in-search-path)))

(install-if-not-installed 'magit)
(define-key global-map (kbd "C-c g") 'magit-file-dispatch)

(install-if-not-installed 'git-gutter-fringe)
;; git-gutter-fringe deferred loading (load alongside eglot's hooks and prog-mode-hook)
(dolist (hook (push 'prog-mode-hook eglot-hooks))
  (add-hook hook (lambda () (interactive)
                   (unless (featurep 'git-gutter-fringe)
                     (require 'git-gutter-fringe)
                     (global-git-gutter-mode t)))))
