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
   dotspacemacs-configuration-layers '(haskell javascript emmanuel git html markdown)
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
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Specify the startup banner. If the value is an integer then the
   ;; banner with the corresponding index is used, if the value is `random'
   ;; then the banner is chosen randomly among the available banners, if
   ;; the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'random
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

  (require 'auto-complete)
  (add-hook 'after-change-major-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . text-mode))
  (define-key evil-normal-state-map (kbd "<f6>") 'evil-search-highlight-persist-remove-all)
  (define-key evil-normal-state-map (kbd "<f5>") 'toggle-truncate-lines)

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

  ;; can't override C-x if only because of C-x b to list buffers.
  (require 'evil-numbers)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "gw") 'transpose-words)

  (setq evil-want-fine-undo nil)

  ;; Obliterate trailing whitespaces before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; display tabs with a dark gray "."
  ;; then the tab itself.
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
  (define-key text-mode-map (kbd "TAB") 'self-insert-command)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
