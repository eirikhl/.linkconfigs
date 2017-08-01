;;;; Package stuff
;; Melpa = package managing and stuff
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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

;; Allow unmarked use of region commands, e.g. copy single line
(use-package whole-line-or-region
  :ensure t
  :defer t)
(whole-line-or-region-mode 1)

;;; Manually included libraries
;; Directory for arbitrary libraries
(add-to-list 'load-path "~/.emacs.d/libs")

;; Text highlighting
(load-library "markerpen")
;; Loadsa stringy stuff
(load-library "s")


;;;; Various keybindings I think make sense
;;; Important note! C-m == enter/return
;; Set Ctrl+q == Ctrl+x, for Dvorak practicality
(global-set-key (kbd "C-q") ctl-x-map) 

(global-set-key (kbd "M-å") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)

(global-set-key (kbd "C-a") 'whole-line-or-region-yank) ; paste
(global-set-key (kbd "C-,") 'whole-line-or-region-kill-ring-save) ; copy
(global-set-key (kbd "M-,") 'whole-line-or-region-kill-region) ; cut

(global-set-key (kbd "C-æ") 'undo)

(global-set-key (kbd "C-|") 'delete-backward-char)

(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-q q") 'kill-buffer)

(global-set-key (kbd "M-o") 'goto-line)

(global-set-key (kbd "C-q C-.") 'eval-region)

(global-set-key (kbd "C-q C-o") 'save-buffer)
(global-set-key (kbd "C-q C-u") 'find-file)

(global-set-key (kbd "C-q C-j") 'save-buffers-kill-emacs)

(global-set-key (kbd "M-q") 'execute-extended-command) ; == M-x

(global-set-key (kbd "<escape> C-å") 'keyboard-escape-quit)


;;;; Convenience and quality of life, random stuff in general
;; Move current line one space up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-c") 'move-line-up)
;; Move current line one space down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-t") 'move-line-down)
;; Navigation commands
(global-set-key (kbd "C-r") 'previous-line)
(global-set-key (kbd "C-t") 'next-line)
;(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-n") 'forward-char)

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
(bind-key* "C-M-<tab>" 'bswindow)

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

;; Easier-access names for highlighting
(defun markred ()
  (interactive)
  (markerpen-mark-region 1))
(defun markyellow ()
  (interactive)
  (markerpen-mark-region 3))
(defun markblue ()
  (interactive)
  (markerpen-mark-region 4))
(defun markgreen ()
  (interactive)
  (markerpen-mark-region 5))

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

;;;; Random stuff I guess?
;; Make scrolling make sense
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;;;; Visual stuff
;; Change default colour theme
; (base16-isotope t) is also nice
(load-theme 'tango-dark)
;; Change cursor and line highlighting
(set-default 'cursor-type 'bar)
(global-hl-line-mode 1)

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
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 83 :width normal))))
 '(hl-line ((t (:background "dim gray" :underline t)))))
