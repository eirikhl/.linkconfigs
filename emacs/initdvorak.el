;;;; Finally not fucking up my fonts?
(add-to-list 'default-frame-alist
                       '(font . "DejaVu Sans Mono-10"))

;;;; In case of future annoyances:
;;;  Change 'allow-unsigned to nil
;;;  Run M-x package-install RET gnu-elpa-keyring-update
;;;  Change back
(setq package-check-signature 'allow-unsigned)


;;;; Debug
(setq debug-on-error t)
; Apparently this has caused all my troubles :( Finally it's over. Keeping as a reminder of evil
;; (setq debug-on-quit t)
;;;; Various keybindings I think make sense
;;; Important note! C-m == enter/return
(global-unset-key (kbd "C-|"))


;;;; Package stuff

;;; Manually included libraries
;; Directory for arbitrary libraries
(add-to-list 'load-path "~/.emacs.d/libs")

; Make sure to download the following libraries manually
(load-library "whole-line-or-region")
(whole-line-or-region-mode 1)
(load-library "s")
;(load-library "perl-completion") requires "anything.el", not bothering yet

;;; Melpa = package managing and stuff
(package-initialize nil)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))


 ;; Install missing packages
 (unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
 (require 'use-package))
(setq use-package-always-ensure t)
;; Auto-complete thingamajig
(unless (package-installed-p 'auto-complete)
 (package-refresh-contents)
 (package-install 'auto-complete))
(ac-config-default)

;; Ivy does fancy shit
(use-package ivy)
(ivy-mode 1)
(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(require 'ivy-rich)
(ivy-rich-mode 1)

;; Automatically install swiper
(unless (package-installed-p 'swiper)
 (package-refresh-contents)
 (package-install 'swiper))
(global-set-key (kbd "C-o") 'swiper)


;;;; Various keybindings I think make sense
;;; Important note! C-m == enter/return
(global-unset-key (kbd "C-|"))

;; Macro to set a list of keybindings and functions
(defmacro gsk (l)
  `(progn ,@(mapcar (lambda (l)
		      `(global-set-key
			(kbd ,(car l)) ,(cadr l))) l)))

;; Using said macro
(gsk (("C-q" ctl-x-map) ; map C-q to C-x
      ("M-å" 'previous-buffer)
      ("M-." 'next-buffer)
      ("C-a" 'whole-line-or-region-yank) ; paste
      ("C-," 'whole-line-or-region-kill-ring-save) ; copy
      ("M-," 'whole-line-or-region-kill-region) ; cut
      ("C-æ" 'undo)
      ("C-|" 'delete-backward-char)
      ("C-k" 'kill-line)
      ("C-q q" 'kill-buffer)
      ("M-o" 'goto-line)
      ("C-q C-." 'eval-region)
      ("C-q C-o" 'save-buffer)
      ("C-q C-u" 'find-file)
      ("C-q C-j" 'save-buffers-kill-emacs) ; exit emacs
      ("M-q" 'execute-extended-command) ; map M-q to M-x
      ("<escape> <escape>" 'keyboard-escape-quit)
      ("C-g" 'top-level)
      ("C-M-s" 'magit-status)
      ("C-h" 'previous-line)
      ("C-t" 'next-line)
      ("C-n" 'backward-char)
      ("C-r" 'backward-word)
      ("C-s" 'forward-char)
      ("C-l" 'forward-word)
      ("C-v" 'move-beginning-of-line)
      ("C-z" 'move-end-of-line)
      ("C-M-<left>" 'shrink-window-horizontally)
      ("C-M-<right>" 'enlarge-window-horizontally)
      ("M-|" 'other-window)
      ("C-q C-u" 'counsel-find-file)
      ))




;;;; Convenience and quality of life, random stuff in general
;; Autocomplete
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                     (if (not (minibufferp (current-buffer)))
                     (auto-complete-mode 1))
                     ))
(real-global-auto-complete-mode t)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 7)

;; Move current line one space up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-h") 'move-line-up)
;; Move current line one space down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-t") 'move-line-down)

;; Toggle comments on one or several lines
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))
(bind-key "C-j" 'comment-or-uncomment-region-or-line)

;; Cycle through open "tiles"
(defun swindow()
  (interactive)
  (other-window 1 t))
;  (select-frame-set-input-focus (selected-frame))) For multiple displays
(defun bswindow()
  (interactive)
  (other-window -1 t))
(bind-key* "C-<tab>" 'swindow)
(bind-key* "C-<iso-lefttab>" 'bswindow)

;; Make emacs add matching parenthesis
;; enable skeleton-pair insert globally
(setq skeleton-pair t)
;;(setq skeleton-pair-on-word t)
;; Uncomment if curly braces won't close in .R files
;; https://github.com/emacs-ess/ESS/issues/296#issuecomment-189614821
;;(define-key ess-mode-map (kbd "{") nil)
;;(define-key ess-mode-map (kbd "}") nil)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; Automatically save and restore sessions
; Should be disabled when working on customisation!
(setq desktop-dirname             "~/.emacs.d/desktop/"
     desktop-base-lock-name      "lock"
     desktop-path                (list desktop-dirname)
     desktop-save                t
     desktop-files-not-to-save   "^$" ;reload tramp paths
     desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(desktop-save-mode 1)

;; Restart emacs from within emacs
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))
(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))
(defun restart-emacs ()
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook
				 (list
				  (if (display-graphic-p)
				      #'launch-separate-emacs-under-x
				    #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))
(global-set-key (kbd "C-q C-p") 'restart-emacs)
(global-set-key (kbd "C-M-p") 'restart-emacs)

;; Unholy abomination which should display total line number
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
                         (let ((str "L%l"))
                           (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                             (setq str (concat str "/" my-mode-line-buffer-line-count)))
                           str)))
                "  %p"
                (list 'column-number-mode "  C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))
(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))
(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)


;; "Smart"/IntelliJ-style Ctrl-backspace
(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))
(global-set-key  [C-backspace]
            'aborn/backward-kill-word)


;; C++-mode indentation
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;;; Random stuff I guess?
;; Make scrolling make sense
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;;;; Visual stuff
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Display line numbers
(global-linum-mode t)
;; Disable startup message
(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (ivy-rich ivy counsel magit gnu-elpa-keyring-update use-package)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
