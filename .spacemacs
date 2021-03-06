;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   ;; for the csv layer must install https://github.com/jb55/spacemacs-csv
   ;; autocompletion tuning, see
   ;; https://github.com/syl20bnr/spacemacs/tree/master/contrib/auto-completion
   dotspacemacs-configuration-layers
   '((
      auto-completion :variables auto-completion-enable-help-tooltip t)
      syntax-checking haskell javascript emmanuel git html
      markdown emacs-lisp shell sql idris helm
      (version-control :variables version-control-side 'left)
      windows-scripts java typescript org racket)
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
                         material
                         planet
                         sanityinc-tomorrow-bright
                         sanityinc-tomorrow-eighties
                         subatomic
                         monokai
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn
                         spacemacs-light
                         spacemacs-dark)
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
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   dotspacemacs-enable-paste-transient-state t)
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

  (spacemacs|do-after-display-system-init
   ;; the fringe having a different background color than the
   ;; frame looks ugly to me, especially when you start splitting
   ;; the window with several buffers
   ;; http://emacs.stackexchange.com/a/5343/2592
   (set-face-attribute 'fringe nil
                       :foreground (face-foreground 'default)
                       :background (face-background 'default))

   ;; if I don't say anything with emacs client i get the arrow...
   (setq powerline-default-separator 'nil)
   (spaceline-compile)

   ;; the planet theme is overdoing the "highlight the current line" big time.
   ;; (global-hl-line-mode 0)

   ;; no box around the modeline. I find it ugly with the tomorrow night theme.
   ;; http://stackoverflow.com/questions/16763393/emacs-24-mode-line-style
   (set-face-attribute 'mode-line nil
                       :overline nil
                       :underline nil
                       :box nil)
  )

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
    (eshell))
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
    (keep-lines txt (point-min) (point-max)))
  (define-key evil-normal-state-map (kbd "<SPC>og") 'emmanuel/keep-lines-ex)
  (defun emmanuel/flush-lines-ex (txt)
    "Same as flush-lines but operate on the whole buffer,
     not only after the cursor."
    (interactive "sEnter the text to grep for: ")
    (flush-lines txt (point-min) (point-max)))
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
      (delete-trailing-whitespace)))
  ;; Obliterate trailing whitespaces before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace-unless-csv)

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

  ;; see http://emacs.stackexchange.com/questions/22740/disable-on-click-mail-sending-in-java-mode
  (add-hook 'java-mode-hook (lambda () (goto-address-mode -1)))

  (require 'git-gutter-fringe+)
  (setq git-gutter-fr+-side 'left-fringe)
  (set-face-foreground 'git-gutter+-modified "orange")

  ;; refresh all the buffers on focus-in
  (add-hook
   'focus-in-hook
   (lambda ()
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (git-gutter+-refresh)))))

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

  (global-company-mode)

  ;; use web-mode to open XML files. nXML is a disaster to me.
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

  ;; gray for html tags in web-mode by default? SERIOUSLY?
  (add-hook 'web-mode-hook
            (lambda ()
              (set-face-attribute 'web-mode-html-tag-face nil :weight 'bold :foreground "#81a2be")))

  ;; tooltips disappear too fast. https://github.com/flycheck/flycheck-pos-tip
  (setq flycheck-pos-tip-timeout 60)

  ;; tune ace-window
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

  ;; must install the fira-code symbol font => https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
  ;;; Fira code
  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  ;; This works when using emacs without server/client
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  ;; I haven't found one statement that makes both of the above situations work, so I use both for now

  (defconst fira-code-font-lock-keywords-alist
    (mapcar (lambda (regex-char-pair)
              `(,(car regex-char-pair)
                (0 (prog1 ()
                     (compose-region (match-beginning 1)
                                     (match-end 1)
                                     ;; The first argument to concat is a string containing a literal tab
                                     ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
            '(("\\(www\\)"                   #Xe100)
              ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
              ("\\(\\*\\*\\*\\)"             #Xe102)
              ("\\(\\*\\*/\\)"               #Xe103)
              ("\\(\\*>\\)"                  #Xe104)
              ("[^*]\\(\\*/\\)"              #Xe105)
              ("\\(\\\\\\\\\\)"              #Xe106)
              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
              ("\\({-\\)"                    #Xe108)
              ("\\(\\[\\]\\)"                #Xe109)
              ("\\(::\\)"                    #Xe10a)
              ("\\(:::\\)"                   #Xe10b)
              ("[^=]\\(:=\\)"                #Xe10c)
              ("\\(!!\\)"                    #Xe10d)
              ("\\(!=\\)"                    #Xe10e)
              ("\\(!==\\)"                   #Xe10f)
              ("\\(-}\\)"                    #Xe110)
              ("\\(--\\)"                    #Xe111)
              ("\\(---\\)"                   #Xe112)
              ("\\(-->\\)"                   #Xe113)
              ("[^-]\\(->\\)"                #Xe114)
              ("\\(->>\\)"                   #Xe115)
              ("\\(-<\\)"                    #Xe116)
              ("\\(-<<\\)"                   #Xe117)
              ("\\(-~\\)"                    #Xe118)
              ("\\(#{\\)"                    #Xe119)
              ("\\(#\\[\\)"                  #Xe11a)
              ("\\(##\\)"                    #Xe11b)
              ("\\(###\\)"                   #Xe11c)
              ("\\(####\\)"                  #Xe11d)
              ("\\(#(\\)"                    #Xe11e)
              ("\\(#\\?\\)"                  #Xe11f)
              ("\\(#_\\)"                    #Xe120)
              ("\\(#_(\\)"                   #Xe121)
              ("\\(\\.-\\)"                  #Xe122)
              ("\\(\\.=\\)"                  #Xe123)
              ("\\(\\.\\.\\)"                #Xe124)
              ("\\(\\.\\.<\\)"               #Xe125)
              ("\\(\\.\\.\\.\\)"             #Xe126)
              ("\\(\\?=\\)"                  #Xe127)
              ("\\(\\?\\?\\)"                #Xe128)
              ("\\(;;\\)"                    #Xe129)
              ("\\(/\\*\\)"                  #Xe12a)
              ("\\(/\\*\\*\\)"               #Xe12b)
              ("\\(/=\\)"                    #Xe12c)
              ("\\(/==\\)"                   #Xe12d)
              ("\\(/>\\)"                    #Xe12e)
              ("\\(//\\)"                    #Xe12f)
              ("\\(///\\)"                   #Xe130)
              ("\\(&&\\)"                    #Xe131)
              ("\\(||\\)"                    #Xe132)
              ("\\(||=\\)"                   #Xe133)
              ("[^|]\\(|=\\)"                #Xe134)
              ("\\(|>\\)"                    #Xe135)
              ("\\(\\^=\\)"                  #Xe136)
              ("\\(\\$>\\)"                  #Xe137)
              ("\\(\\+\\+\\)"                #Xe138)
              ("\\(\\+\\+\\+\\)"             #Xe139)
              ("\\(\\+>\\)"                  #Xe13a)
              ("\\(=:=\\)"                   #Xe13b)
              ("[^!/]\\(==\\)[^>]"           #Xe13c)
              ("\\(===\\)"                   #Xe13d)
              ("\\(==>\\)"                   #Xe13e)
              ("[^=]\\(=>\\)"                #Xe13f)
              ("\\(=>>\\)"                   #Xe140)
              ("\\(<=\\)"                    #Xe141)
              ("\\(=<<\\)"                   #Xe142)
              ("\\(=/=\\)"                   #Xe143)
              ("\\(>-\\)"                    #Xe144)
              ("\\(>=\\)"                    #Xe145)
              ("\\(>=>\\)"                   #Xe146)
              ("[^-=]\\(>>\\)"               #Xe147)
              ("\\(>>-\\)"                   #Xe148)
              ("\\(>>=\\)"                   #Xe149)
              ("\\(>>>\\)"                   #Xe14a)
              ("\\(<\\*\\)"                  #Xe14b)
              ("\\(<\\*>\\)"                 #Xe14c)
              ("\\(<|\\)"                    #Xe14d)
              ("\\(<|>\\)"                   #Xe14e)
              ("\\(<\\$\\)"                  #Xe14f)
              ("\\(<\\$>\\)"                 #Xe150)
              ("\\(<!--\\)"                  #Xe151)
              ("\\(<-\\)"                    #Xe152)
              ("\\(<--\\)"                   #Xe153)
              ("\\(<->\\)"                   #Xe154)
              ("\\(<\\+\\)"                  #Xe155)
              ("\\(<\\+>\\)"                 #Xe156)
              ("\\(<=\\)"                    #Xe157)
              ("\\(<==\\)"                   #Xe158)
              ("\\(<=>\\)"                   #Xe159)
              ("\\(<=<\\)"                   #Xe15a)
              ("\\(<>\\)"                    #Xe15b)
              ("[^-=]\\(<<\\)"               #Xe15c)
              ("\\(<<-\\)"                   #Xe15d)
              ("\\(<<=\\)"                   #Xe15e)
              ("\\(<<<\\)"                   #Xe15f)
              ("\\(<~\\)"                    #Xe160)
              ("\\(<~~\\)"                   #Xe161)
              ("\\(</\\)"                    #Xe162)
              ("\\(</>\\)"                   #Xe163)
              ("\\(~@\\)"                    #Xe164)
              ("\\(~-\\)"                    #Xe165)
              ("\\(~=\\)"                    #Xe166)
              ("\\(~>\\)"                    #Xe167)
              ("[^<]\\(~~\\)"                #Xe168)
              ("\\(~~>\\)"                   #Xe169)
              ("\\(%%\\)"                    #Xe16a)
              ("[0-9]\\(x\\)[0-9a-fA-F]"     #Xe16b)
              ("[^:=]\\(:\\)[^:=]"           #Xe16c)
              ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
              ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords)

  ;; https://www.emacswiki.org/emacs/DeadKeys
  (require 'iso-transl)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; JAVA STUFF FOR WORK
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq eclim-eclipse-dirs "/"
        ;; eclim-executable "~/.eclipse/org.eclipse.platform_793567567_linux_gtk_x86_64/eclim")
        eclim-executable "~/Downloads/eclipse/plugins/org.eclim_2.6.0/bin/eclim")

  ;; closer to company style guide for java code.
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Indenting-switch-statements.html
  (c-set-offset 'case-label '+)

  ;; don't align arguments with the opening bracket.
  ;; http://stackoverflow.com/a/6952408/516188
  (c-set-offset 'arglist-intro '++)

  (require 'compile)

  ;; clean for the parent project.
  (defun java-clean-all ()
    (interactive)
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml clean install -Pcompile-ts -DskipTests"))

  (defun java-build-all ()
    (interactive)
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml install -Pcompile-ts -DskipTests"))

  ;; clean for core
  (defun java-clean-generic-deps ()
    (interactive)
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml clean install -pl :generic_tms -am -DskipTests"))

  ;; build for core
  (defun java-generic-deps ()
    (interactive)
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml install -pl :generic_tms -am -DskipTests"))

  ;; clean the current project, but with the linux-dev profile.
  (defun java-clean-linux ()
    (interactive)
    (eclim--maven-execute " -Plinux-dev clean install -DskipTests"))

  (defun path-components ()
    (split-string (file-name-directory buffer-file-name) "/"))

  (defun java-cur-package-name ()
    (mapconcat 'identity (cdr (-drop-while (lambda (str) (not (string= "java" str)))
     (-butlast (path-components)))) "."))

  (defun maven-project-root-folder ()
    (concat "/" (mapconcat 'identity
               (cdr (reverse (cdr (-drop-while
                          (lambda (str) (not (string= "src" str)))
                          (reverse (path-components)))))) "/")))

  ;; (eclim-package-and-class) ;; doesn't work, picks the first inner class
  (defun java-cur-package-and-class ()
    (concat (java-cur-package-name)
            "."
            (file-name-base buffer-file-name)))

  (defun java-base-test-file (target)
    (interactive)
    (eclim--maven-execute
     (concat
      "-Dtest="
      (java-cur-package-and-class)
      " -Plinux-dev,all-tests "
      target)))


  ;; tests for java-cur-method-name
  ;; private void createDistributionTestCheck(DistributionCreateDto createDto,
  ;;                                                                DistributionDto distributionDto, CreateDistributionPrepare preparedData) {
  ;;
  ;; public void testCreate() throws Exception {
  ;;
  ;; public void realFunction() throws Exception {
  ;;   lambdaCall("bla", () -> {
  ;;
  ;; new ClassName()

  ;; emacs-eclim is supposed to have eclim-java-method-signature-at-point
  ;; but it's not implemented.
  (defun java-cur-method-name ()
    (save-excursion (if (re-search-backward " \\(\\w+\\) \\(\\w+\\)(\\(.\\|\n\\)*)\\(.\\|\n\\)*{")
                        (if (string= "new" (match-string-no-properties 1))
                            (java-cur-method-name)
                          (match-string-no-properties 2))
                      "")))

  (defun java-test-method ()
    (interactive)
    (eclim--maven-execute
     (concat
      "-Dtest="
      (java-cur-package-and-class)
      "#"
      (java-cur-method-name)
      " -Plinux-dev,all-tests test")))

  (defun java-debug-test-method ()
    (interactive)
    (compile
     (concat
      "mvnDebug -f /home/emmanuel/projects/bus/generic/generic_tms/pom.xml " ;; TODO unhardcode project path.
      "-Dtest="
      (java-cur-package-and-class)
      "#"
      (java-cur-method-name)
      " -Plinux-dev,all-tests test")))

  (defun java-clean-test-file ()
    (interactive)
    (java-base-test-file "clean test"))
  (defun java-test-file ()
    (interactive)
    (java-base-test-file "test"))

  (defun java-clean-test-all ()
    (interactive)
    (shell-command-to-string "sh -c 'cd /home/emmanuel/projects/bus/generic/ims && sh ./copy_core.sh'")
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml -fae -Plinux-dev,all-tests,compile-ts  clean install"))

  (defun java-clean-test-all-no-migrations ()
    (interactive)
    (shell-command-to-string "sh -c 'cd /home/emmanuel/projects/bus/generic/ims && sh ./copy_core.sh'")
    (compile "mvn -f /home/emmanuel/projects/bus/generic/pom.xml -fae -Plinux-dev,all-tests,compile-ts  -Dall-tests.exclude=**/*MigrationTest.java,**/TripEngineTest.java,**/IdGeneratorTest.java,**/PerformanceEngineTest.java clean install"))

  (defun java-create-type (main-test package type type-name)
    (interactive
     (list (ido-completing-read "Main or test file?" '("main" "test"))
           ;; TODO change this to list all packages in the app,
           ;; with option to type in, like ",ri"
           (read-string "Package: " (java-cur-package-name))
           (ido-completing-read "Type?" '("class" "interface" "abstract class" "enum" "@interface"))
           (read-string "Type name: ")))
     (let* ((rel-fname (concat (replace-regexp-in-string "\\." "/" package) "/" type-name ".java"))
            (abs-fname (concat (maven-project-root-folder) "/src/" main-test "/java/" rel-fname)))
       (find-file abs-fname)
       (insert (concat "package " package ";\n\npublic " type " " type-name " {\n\n}"))))

  (defun full-project-path (path)
    (concat "/home/emmanuel/projects/bus/generic/" path))

  (defun project-sources (path)
    (list (concat path "/src/main/java") (concat path "/src/test/java")))

  (require 'dash-functional)

  (defun java-debug-attach ()
      (interactive)
      (let* ((folders (-mapcat
                       (-compose 'project-sources 'full-project-path)
                       '("generic_tms" "core")))
             (sourcepath (mapconcat 'identity folders ":")))
        (jdb (concat "jdb -attach 8000 -sourcepath" sourcepath))))

  ;; pasted from https://github.com/syl20bnr/spacemacs/pull/2554
  ;; same as the md shortcuts in the set-key-from-mode.
  (spacemacs|define-micro-state eclim
    :doc "[b] break [r] run [n] next [s] step [c] cont [i] inspect [q] quit"
    :disable-evil-leader t
    :persistent t
    :evil-leader-for-mode (java-mode . "md.")
    :bindings
    ("b" gud-break)
    ("r" gud-run)
    ("n" gud-next)
    ("s" gud-step)
    ("i" gud-print)
    ("c" gud-cont)
    ("q" nil :exit t))

  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    "oa" 'java-create-type
    "oC" 'java-clean-all
    "oA" 'java-build-all
    "ok" 'java-generic-deps
    "oK" 'java-clean-generic-deps
    "ocl" 'java-clean-linux
    "otA" 'java-clean-test-all
    "ota" 'java-clean-test-all-no-migrations
    "otf" 'java-test-file
    "otF" 'java-clean-test-file
    "otm" 'java-test-method
    "otM" 'java-debug-test-method
    "od" 'java-debug-attach
    "db" 'gud-break
    "dc" 'gud-cont
    "dn" 'gud-next
    "dr" 'gud-run
    "ds" 'gud-step
    "df" 'gud-finish
    "di" 'gud-print
    "dl" 'spacemacs/gud-locals
    "dd" 'eclim-debug-test
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; END JAVA STUFF FOR WORK
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.
  (custom-set-variables
   '(flycheck-indication-mode (quote left-fringe))
   '(haskell-indentation-ifte-offset 4)
   '(haskell-indentation-layout-offset 4)
   '(haskell-indentation-left-offset 4)
   )
  )
