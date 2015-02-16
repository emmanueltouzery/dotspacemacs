;;; packages.el --- emmanuel Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar emmanuel-packages
  '(
    ;; package emmanuels go here
    color-theme-sanityinc-tomorrow
    qml-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar emmanuel-excluded-packages '()
  "List of packages to exclude.")


(defun emmanuel/init-qml-mode ()
    (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  	(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
  )

;; For each package, define a function emmanuel/init-<package-emmanuel>
;;
;; (defun emmanuel/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
