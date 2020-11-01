;;; Load file for batch emacs to generate PostScript for Ada source.

(add-to-list 'load-path (expand-file-name "~/Gnuemacs"))       ; my extensions, settings
(add-to-list 'load-path (concat "e:/Gnu/Emacs/site-lisp"))  ; standard add-on packages; ada-mode

(require 'ada-mode)
(require 'ps-print)
(require 'font-lock)
(global-font-lock-mode t)

(load "ada-mode-keys") ;; defines ada-ps-buffer

;;; end of file

