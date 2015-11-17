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
   dotspacemacs-configuration-layers
   '(auto-completion syntax-checking haskell javascript emmanuel git html
                     markdown evil-snipe csv emacs-lisp shell version-control
                     windows-scripts java)
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

  ;; wider default window
  ;; http://stackoverflow.com/a/12334932/516188
  (add-to-list 'default-frame-alist '(width . 100))

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

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; if I don't say anything with emacs client i get the arrow...
  (setq powerline-default-separator 'wave)

  ;; show trailing whitespace (and in a way compatible with fill-column-indicator)
  (setq whitespace-style '(face trailing))

  ;; fill-column-indicator

  ;; restore the default rule color from the tomorrow-night theme,
  ;; I find the one spacemacs sets up ugly.
  (setq fci-rule-color "#373b41")

  ;; enable fill-column-indicator for all major modes
  ;; except web-mode where it's sadly currently buggy:
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/46
  (add-hook 'after-change-major-mode-hook
            (lambda () (if (string= major-mode "web-mode")
                (turn-off-fci-mode) (turn-on-fci-mode))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; fill-column-indicator has problems.
  ;; workarounds => http://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80

  ;; autocomplete workaround
  (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))

  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; company-mode workaround
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  ;; end fill-column-indicator workarounds
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;; http://emacs.stackexchange.com/questions/13485
  (defun delete-frame-or-kill-emacs ()
    "Delete the selected frame.  If the last one, kill Emacs."
    (interactive)
    (condition-case nil (delete-frame) (error (save-buffers-kill-terminal))))
  (define-key evil-normal-state-map (kbd "<SPC>oF") 'delete-frame-or-kill-emacs)

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

  ;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  (defun sudo-edit (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
  (define-key evil-normal-state-map (kbd "<SPC>or") 'sudo-edit)

  ;; http://stackoverflow.com/a/20137832/516188
  (defun save-all () (interactive) (save-some-buffers t))
  (define-key evil-normal-state-map (kbd "<SPC>oW") 'save-all)

  (defun gnome-open-directory ()
    (interactive)
    "gnome-opens the current directory."
    (let ((process-connection-type nil))
      (start-process "" nil "/usr/bin/gnome-open" default-directory)))
    (define-key evil-normal-state-map (kbd "<SPC>od") 'gnome-open-directory)

  ;; can't override C-x if only because of C-x b to list buffers.
  (require 'evil-numbers)
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "gw") 'transpose-words)

  ;; undo boundary at normal mode entry/exit
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

  ;; http://emacs.stackexchange.com/questions/17754/insert-word-under-cursor-in-projectile-search
  (with-eval-after-load 'helm (define-key helm-map (kbd "C-w") 'next-history-element))

  ;; http://emacswiki.org/emacs/SearchAtPoint#toc6
  ;; Move to beginning of word before yanking word in isearch-mode.
  ;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
  ;; C-s C-w [C-w] [C-w]... behaviour.

  (require 'thingatpt)

  (defun my-isearch-yank-word-or-char-from-beginning ()
    "Move to beginning of word before yanking word in isearch-mode."
    (interactive)
    ;; Making this work after a search string is entered by user
    ;; is too hard to do, so work only when search string is empty.
    (if (= 0 (length isearch-string))
        (beginning-of-thing 'word))
    (isearch-yank-word-or-char)
    ;; Revert to 'isearch-yank-word-or-char for subsequent calls
    (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
                               'isearch-yank-word-or-char
                               isearch-mode-map))

  (add-hook 'isearch-mode-hook
            (lambda ()
              "Activate my customized Isearch word yank command."
              (substitute-key-definition 'isearch-yank-word-or-char
                                         'my-isearch-yank-word-or-char-from-beginning
                                         isearch-mode-map)))

  ;; end http://emacswiki.org/emacs/SearchAtPoint#toc6

  ;; insert real tabs in text-mode
  ;; (for instance editing CSV files)
  ;; http://vserver1.cscs.lsa.umich.edu/~rlr/Misc/emacs_tabs.htm
  ;; for some reason for text-mode it works better WITHOUT the eval-after-load
  ;; and for csv-mode it works better WITH it...
  (define-key text-mode-map (kbd "TAB") 'self-insert-command)
  (eval-after-load 'csv-mode
    '(define-key csv-mode-map (kbd "TAB") 'self-insert-command))

  (setq vc-follow-symlinks t)
  (setq diff-hl-side 'left)
  ;; https://github.com/syl20bnr/spacemacs/issues/2180
  (diff-hl-margin-mode 0)
  ;; hopefully update the git modifications display in the
  ;; fringe if I commit outside of emacs.
  (add-hook 'focus-in-hook (lambda () (diff-hl-update)))

  ; http://www.emacswiki.org/emacs/SavePlace
  (require 'saveplace)
  (setq-default save-place t)

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

  ;; get flyway to shut up about this warning,
  ;; coming up all the time with the flyway for haskell.
  ;; https://raw.githubusercontent.com/flycheck/flycheck/master/flycheck.el

(defun flycheck-finish-checker-process-no-suspicious
    (checker exit-status files output callback)
  "Finish a checker process from CHECKER with EXIT-STATUS.

FILES is a list of files given as input to the checker.  OUTPUT
is the output of the syntax checker.  CALLBACK is the status
callback to use for reporting.

Parse the OUTPUT and report an appropriate error status."
  (let ((errors (flycheck-parse-output output checker (current-buffer))))
    (funcall callback 'finished
             ;; Fix error file names, by substituting them backwards from the
             ;; temporaries
             (mapcar (lambda (e) (flycheck-fix-error-filename e files))
                     errors))))

  (defun flycheck-ignore-errors (orig-func &rest args)
    "Ignore flycheck errors"
    (apply 'flycheck-finish-checker-process-no-suspicious args))

  (advice-add 'flycheck-finish-checker-process :around
              #'flycheck-ignore-errors)

  ;; end get rid of flyway warning.

  ;; autocompletion tuning, see
  ;; https://github.com/syl20bnr/spacemacs/tree/master/contrib/auto-completion
  (setq-default dotspacemacs-configuration-layers
                '(auto-completion :variables
                                  auto-completion-enable-help-tooltip t))

  (global-company-mode)

(setq eclim-eclipse-dirs "/"
      eclim-executable "~/.eclipse/org.eclipse.platform_793567567_linux_gtk_x86_64/eclim")

   ;; Do not write anything past this comment. This is where Emacs will
   ;; auto-generate custom variable definitions.
(custom-set-variables
 '(flycheck-indication-mode (quote right-fringe))
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 )
)
