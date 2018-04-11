;;;; Debug
(setq debug-on-error t)
(setq debug-on-quit t)

;;;; Package stuff
;; Melpa = package managing and stuff
(package-initialize)
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

;; Install missing packages
; Guess it doesn't work like I want it to?
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;; Auto-complete thingamajig
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents)
  (package-install 'auto-complete))
(ac-config-default)
;; Automatically install base16
(unless (package-installed-p 'base16-theme)
  (package-refresh-contents)
  (package-install 'base16-theme))
;; Automatically install swiper
(unless (package-installed-p 'swiper)
  (package-refresh-contents)
  (package-install 'swiper))
;; Ivy does fancy shit
(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-o") 'swiper)

(whole-line-or-region-mode 1)

;; Emacs Code Browser, basically OP shit
;(require 'ecb)
;(require 'ecb-autoloads) I want this...

;;; Manually included libraries
;; Directory for arbitrary libraries
(add-to-list 'load-path "~/.emacs.d/libs")

;; Loadsa stringy stuff
(load-library "s")


;;;; Various keybindings I think make sense
;;; Important note! C-m == enter/return
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
      ("<escape> C-å" 'keyboard-escape-quit)
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
      ))


;;;; Convenience and quality of life, random stuff in general
;; Spell-checking
(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Collapse parts of the text
(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-g") ; Or something else

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
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

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
;; Change default colour theme
;; Different ones I think look kinda nice
; (load-theme 'base16-isotope t)
; (load-theme 'tango-dark)
; (load-theme 'base16-railscasts)
; (load-theme 'adwaita)

(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; Change cursor and line highlighting
(set-default 'cursor-type 'bar)
;; (global-hl-line-mode 1)

;; Highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Highlight numbers and function calls
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'highlight-function-calls-mode)


;; Disable scroll bar
(scroll-bar-mode -1)
;; Display line numbers
(global-linum-mode t)
;; Disable startup message
(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#f5f7ff" "#c94922" "#ac9739" "#c08b30" "#3d8fd1" "#6679cc" "#3d8fd1" "#5e6687"])
 '(ansi-term-color-vector
   [unspecified "#f5f7ff" "#c94922" "#ac9739" "#c08b30" "#3d8fd1" "#6679cc" "#3d8fd1" "#5e6687"])
 '(custom-safe-themes
   (quote
    ("a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" default)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 83 :width normal))))
 '(hl-line ((t (:background "dim gray")))))


;;;; JDEE
;; (add-to-list 'load-path (format "%s/dist/jdee-2.4.1/lisp" "~/.emacs.d/libs/"))
;; (autoload 'jde-mode "jde" "JDE mode" t)
;; (setq auto-mode-alist
      ;; (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))

