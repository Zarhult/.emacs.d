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

;;; Themes
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

;; Install some cool themes
(install-if-not-installed 'ewal)
(dolist (theme '(ewal-spacemacs-themes
                 base16-theme
                 doom-themes
                 cherry-blossom-theme
                 minsk-theme))
  (install-if-not-installed theme))
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(setq ewal-shade-percent-difference 15) ; This makes avy's colors readable with ewal theme

;; List of good themes to cycle through, with first theme being default theme
(setq main-themes
      (list 'ewal-spacemacs-classic
            'base16-ashes
            'base16-darkviolet
            'my-manoj-dark
            'doom-ir-black
            'cherry-blossom
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

;; Set first theme
(cycle-theme)

;; Also rand-theme option
(install-if-not-installed 'rand-theme)
(setq rand-theme-unwanted '(adwaita dichromacy tango leuven light-blue tsdh-light modus-operandi whiteboard))
(define-key global-map (kbd "C-c r") 'rand-theme)

;;; Basic configuration
(set-language-environment "Japanese")

;; Enable C-x C-l, C-x C-u
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Highlight matching parens
(show-paren-mode t)

;; Automatically create matching parens
(electric-pair-mode 1)

;; Show column in modeline
(column-number-mode t)

;; Font size
(set-face-attribute 'default nil :height 105)

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

;; Don't auto-fit images
(setq-default image-auto-resize 1.2)
;; Hide mode-line when viewing images
(add-hook 'image-mode-hook (lambda () (setq mode-line-format nil)))
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

;; Make .conf files and extensionless files trigger text-mode
(add-to-list 'auto-mode-alist '("\\.conf\\'" . text-mode))
(add-to-list 'auto-mode-alist '("/[^\\./]*\\'" . text-mode))

;; Use relative line numbers when line numbers are enabled
(setq display-line-numbers-type 'relative)

;; Scroll line by line
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Better asm-mode indentation
(defun my-asm-mode-hook ()
  "Hook to override default `asm-mode' indentation function."
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

;; No warnings when showing lots of images with image-dired-show-all-from-dir
(setq image-dired-show-all-from-dir-max-files 999)

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

(defun alternate-buffer ()
  "Switch to the buffer that was active before the current one."
  (interactive)
  (switch-to-buffer nil))

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

;;; Keybinds - for my functions
(define-key global-map (kbd "C-x 2") 'split-and-follow-horizontally)
(define-key global-map (kbd "C-x 3") 'split-and-follow-vertically)
(define-key global-map (kbd "C-M-'") 'alternate-buffer)
(define-key global-map (kbd "C-c s") 'eshell-other-window)
(define-key global-map (kbd "C-c a") 'ansi-term-other-window)
(define-key global-map (kbd "C-c d") 'dired-other-window-current-directory)
(define-key global-map (kbd "C-c m") 'dired-music-other-window)
(define-key global-map (kbd "C-c i") 'image-dired-current-directory)
(define-key global-map (kbd "C-c ;") 'toggle-mode-line)
(define-key global-map (kbd "C-c '") 'load-theme-from-scratch)
(define-key global-map (kbd "C-c k") 'cycle-theme)
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
(define-key global-map (kbd "C-c h") 'org-timer-map)
(define-key org-timer-map (kbd "s") 'org-timer-set-timer)
(define-key org-timer-map (kbd "p") 'org-timer-pause-or-continue)
(define-key org-timer-map (kbd "k") 'org-timer-stop)

;;; Packages - general
(install-if-not-installed 'hydra)
(setq hydra-is-helpful nil)

(install-if-not-installed 'key-chord)
(setq key-chord-two-keys-delay 0.075)
(key-chord-mode t)

(defun my-jump-to-mark () ; Function that is equivalent to entering C-u SPC
  "Jump to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument,
or using the C-u SPC keybind."
  (interactive)
  (set-mark-command '(4)))

;; Hydra solution to emacs pinky. When activated with keychord "jk", buffer and
;; window navigation becomes possible without needing to hold ctrl. Works much
;; like using vanilla bindings with ctrl pressing itself for you (for navigation only).
(key-chord-define-global
 "jk"
 (defhydra hydra-nav ()
   "nav"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ("s" (lambda () ; isearch-forward without leaving hydra
          (interactive)
          (isearch-forward-regexp)
          (hydra-nav/body))  :color blue)
   ("r" (lambda () ; isearch-backward without leaving hydra
          (interactive)
          (isearch-backward-regexp)
          (hydra-nav/body))  :color blue)
   ("u SPC" my-jump-to-mark) ; same as C-u SPC
   ("M-v" scroll-down-command)
   ("M-b" backward-word)
   ("M-f" forward-word)
   ("M-m" back-to-indentation)
   ("M-n" flymake-goto-next-error)
   ("M-p" flymake-goto-prev-error)
   ("g" keyboard-quit)
   (";" avy-goto-char)
   ("'" avy-goto-line)
   ("l" recenter-top-bottom)
   ("M-<" beginning-of-buffer)
   ("M->" end-of-buffer)
   ("M-r" move-to-window-line-top-bottom)
   ("x0" delete-window)
   ("x1" delete-other-windows)
   ("x2" split-and-follow-vertically)
   ("x3" split-and-follow-horizontally)
   ("xs" save-buffer)
   ("xf" find-file)
   ("xk" kill-this-buffer)
   ("o" other-window)
   ("O" (lambda () ;; same as C-- C-x o
          (interactive)
          (other-window -1)))
   ("q" nil)))

(install-if-not-installed 'writeroom-mode)
(define-key global-map (kbd "C-M-,") 'writeroom-mode)
(setq writeroom-bottom-divider-width 0) ; Don't adjust buffer width/center text
(setq writeroom-width 1)
(setq writeroom-restore-window-config t)
(setq writeroom-fullscreen-effect "maximized")
(with-eval-after-load 'writeroom-mode
  (delete 'writeroom-set-alpha writeroom-global-functions)) ; Don't modify transparency

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
(setq avy-all-windows nil) ; Only consider candidates in the current window
(define-key global-map (kbd "C-;") 'avy-goto-char)
(define-key global-map (kbd "C-'") 'avy-goto-line)
(with-eval-after-load 'avy
  (set-face-attribute 'avy-lead-face nil :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil :weight 'bold))

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

(install-if-not-installed 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Note that for pdf-tools, if on Gentoo, must install
;; app-text/poppler with "cairo" use flag
(install-if-not-installed 'pdf-tools)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode) ; Default dark mode
(pdf-loader-install)

;;; Packages - programming
(install-if-not-installed 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")
;; Hook into lsp-mode for each mode hook listed below.
;; Language servers must be installed externally for lsp to work.
;; C/C++: Must have ccls, clangd, or similar installed
;; html: "npm install -g vscode-html-languageserver-bin"
;; css: "npm install -g vscode-css-languageserver-bin"
;; js: "npm install -g typescript-language-server; npm install -g typescript"
;; python: "pip install 'python-language-server[all]'"
(setq my-lsp-mode-hooks '(c-mode-hook c++-mode-hook objc-mode-hook
                                      html-mode-hook css-mode-hook js-mode-hook
                                      python-mode-hook))
(dolist (hook my-lsp-mode-hooks)
  (add-hook hook #'lsp-deferred))
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;; Set up lsp-pyright deferred loading
(install-if-not-installed 'lsp-pyright)
(defun require-pyright ()
  "Load lsp-pyright."
  (unless (featurep 'lsp-pyright)
    (require 'lsp-pyright)))
(add-hook 'python-mode-hook 'require-pyright)

;; Disable some functionality
;; (setq lsp-enable-on-type-formatting nil)
;; (setq lsp-headerline-breadcrumb-enable nil) ; Hide headerline

;; Enable stricter linting for css
(setq lsp-css-lint-duplicate-properties t
      lsp-css-lint-zero-units t
      lsp-css-lint-universal-selector t
      lsp-css-lint-box-model t
      lsp-css-lint-important t
      lsp-css-lint-float t
      lsp-css-lint-id-selector t)
;; lsp-mode performance boosting
;; Be sure also to use emacs version 27+ compiled with native json support
(setq read-process-output-max (* 1024 1024))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; Put flymake on right fringe so we can use git-gutter on the left fringe
(setq flymake-fringe-indicator-position 'right-fringe)

(install-if-not-installed 'lsp-ui)
(with-eval-after-load 'lsp-ui
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (define-key lsp-ui-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-?") 'lsp-ui-peek-find-references))

(install-if-not-installed 'company)
(setq company-idle-delay nil) ;; Complete only on demand with keybind
(define-key global-map (kbd "M-i") 'company-complete)

;; Need yasnippet for html completion
 (install-if-not-installed 'yasnippet)
 (add-hook 'lsp-mode-hook #'yas-global-mode)

(install-if-not-installed 'projectile)
;; Load and set up projectile only after either lsp-mode is enabled (enter
;; a project) or an attempt at using a projectile keybind is made
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
(add-hook 'lsp-mode-hook
          (lambda () (interactive)
            (setup-projectile-with-prefix my-projectile-prefix)))
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
;; git-gutter-fringe deferred loading
(defun load-git-gutter-fringe ()
  "Load git-gutter-fringe."
  (unless (featurep 'git-gutter-fringe)
    (require 'git-gutter-fringe)
    (global-git-gutter-mode t)))
(add-hook 'prog-mode-hook 'load-git-gutter-fringe) ; todo: why does this cause load at startup?
(add-hook 'text-mode-hook 'load-git-gutter-fringe)
