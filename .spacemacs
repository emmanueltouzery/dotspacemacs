;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   ;; for the csv layer must install https://github.com/jb55/spacemacs-csv
   dotspacemacs-configuration-layers '(auto-completion syntax-checking haskell javascript emmanuel git html markdown evil-snipe csv)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."

  ;; without those two I get toolbar+scrollbar in emacsclient,
  ;; but not plain emacs.
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Specify the startup banner. If the value is an integer then the
   ;; banner with the corresponding index is used, if the value is `random'
   ;; then the banner is chosen randomly among the available banners, if
   ;; the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-tomorrow-night
                         monokai
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`
   dotspacemacs-major-mode-leader-key ","
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil)
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; if I don't say anything with emacs client i get the arrow...
  (setq powerline-default-separator 'wave)

  ;; fill-column-indicator
  (require 'fill-column-indicator)
  (setq fci-rule-column 80)
  (add-hook 'after-change-major-mode-hook 'fci-mode)

  (require 'auto-complete)
  (add-hook 'after-change-major-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . text-mode))
  (bind-key (kbd "<f6>") 'evil-search-highlight-persist-remove-all)
  (define-key evil-normal-state-map (kbd "RET") 'evil-search-highlight-persist-remove-all)
  (bind-key (kbd "<f5>") 'toggle-truncate-lines)
  (bind-key (kbd "C-SPC") 'helm-multi-files)

  ;; http://stackoverflow.com/a/9414763/516188
  (defun emmanuel/prelude-copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))
  (define-key evil-normal-state-map (kbd "<SPC>oc") 'emmanuel/prelude-copy-file-name-to-clipboard)

  (defun emmanuel/open-shell ()
    "Split the window and open a shell in the new split"
    (interactive)
    (split-window-below-and-focus)
    (eshell)
    )
  (define-key evil-normal-state-map (kbd "<SPC>os") 'emmanuel/open-shell)

  (define-key evil-normal-state-map (kbd "<SPC>of") 'make-frame)
  (define-key evil-normal-state-map (kbd "<SPC>ou") 'browse-url)

  (defun emmanuel/keep-lines-ex (txt)
    "Same as keep-lines but operate on the whole buffer,
     not only after the cursor."
    (interactive "sEnter the text to grep for: ")
    (keep-lines txt (point-min) (point-max))
    )
  (define-key evil-normal-state-map (kbd "<SPC>og") 'emmanuel/keep-lines-ex)
  (defun emmanuel/flush-lines-ex (txt)
    "Same as flush-lines but operate on the whole buffer,
     not only after the cursor."
    (interactive "sEnter the text to grep for: ")
    (flush-lines txt (point-min) (point-max))
    )
  (define-key evil-normal-state-map (kbd "<SPC>ov") 'emmanuel/flush-lines-ex)
  (define-key evil-normal-state-map (kbd "<SPC>ow") 'delete-other-windows)

  (defun copy-buffer ()
    "Copy entire buffer to clipboard"
    (interactive)
    (clipboard-kill-ring-save (point-min) (point-max)))
  (define-key evil-normal-state-map (kbd "<SPC>oy") 'copy-buffer)

  ;; can't override C-x if only because of C-x b to list buffers.
  (require 'evil-numbers)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "gw") 'transpose-words)

  (setq evil-want-fine-undo nil)

  ;; http://kalkanotel.com/emacs-24-untabify-on-save-for-everything-except-makefiles-i268813.htm
  (defun delete-trailing-whitespace-unless-csv()
    "Delete trailing whitespace except if the file is CSV"
    (unless (derived-mode-p 'csv-mode)
      (delete-trailing-whitespace))
    )
  ;; Obliterate trailing whitespaces before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace-unless-csv)

  ;; display tabs with a dark gray "."
  ;; then the tab itself.
  ;; http://www.emacswiki.org/emacs/ShowWhiteSpace
  (standard-display-ascii ?\t ".\t")
  (defface extra-whitespace-face
    '((t (:foreground "dim gray")))
    "Used for tabs and such.")
  (defvar my-extra-keywords
    '(("\t" . 'extra-whitespace-face)))
  (add-hook 'font-lock-mode-hook
            (lambda () (font-lock-add-keywords nil my-extra-keywords)))

  ;; insert real tabs in text-mode
  ;; (for instance editing CSV files)
  ;; http://vserver1.cscs.lsa.umich.edu/~rlr/Misc/emacs_tabs.htm
  ;; for some reason for text-mode it works better WITHOUT the eval-after-load
  ;; and for csv-mode it works better WITH it...
  (define-key text-mode-map (kbd "TAB") 'self-insert-command)
  (eval-after-load 'csv-mode
    '(define-key csv-mode-map (kbd "TAB") 'self-insert-command))

  (setq vc-follow-symlinks t)
  (setq git-gutter-fr:side 'left-fringe)

  ; http://www.emacswiki.org/emacs/SavePlace
  (require 'saveplace)
  (setq-default save-place t)

  ;; I often commit outside of emacs using the command-line.
  ;; this is an attempt to force git gutter to update
  ;; the commit status everytime the window is focused in
  (require 'git-gutter)
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)

  ;; getting tired of the cache getting stale, because
  ;; I'm running commands outside of emacs (copying files, pulling
  ;; from git and so on). Performance still looks OK for now.
  (setq projectile-enable-caching nil)

  ;; I want the word boundary to be at spaces, not also at _ or - depending on the major mode. Useful for "yaw", w/b and others.
  ;; https://bitbucket.org/lyro/evil/wiki/Home
  (add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

  ;; i want the line numbers in the modeline always!!!
  ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Optional-Mode-Line.html
  ;; http://emacs.stackexchange.com/a/3827/2592
  (setq line-number-display-limit nil)
  (setq line-number-display-limit-width 250000)

  ;; http://stackoverflow.com/a/145359/516188
  (defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.
     Move point to the first non-whitespace character on this line.
     If point was already at that position, move point to beginning of line."
    (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line))))
  (global-set-key [home] 'smart-beginning-of-line)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 '(flycheck-indication-mode (quote right-fringe))
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 )
