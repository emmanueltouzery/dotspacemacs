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

(setq emmanuel-packages
  '(
    ;; package emmanuels go here
    color-theme-sanityinc-tomorrow
    ))

(setq emmanuel-excluded-packages '())

;; For each package, define a function emmanuel/init-<package-emmanuel>
;;
(defun emmanuel/init-color-theme-sanityinc-tomorrow ()
  "Initialize theme"
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
