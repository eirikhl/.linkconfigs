;; Don't touch this
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Also don't touch this
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;; Package stuff
;; Melpa = package managing and stuff
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install missing packages
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

;; Ivy does fancy shit
(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-o") 'swiper)

;; Directory for arbitrary libraries
(add-to-list 'load-path "~/.emacs.d/libs")

;; Text highlighting
(load-library "markerpen")


;;;; Various keybindings I think make sense
;; Set Ctrl+q == Ctrl+x, for Dvorak practicality
(global-set-key (kbd "C-q") ctl-x-map) 

(global-set-key (kbd "M-å") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)

(global-set-key (kbd "C-a") 'yank) ; paste
(global-set-key (kbd "C-,") 'kill-ring-save) ; copy
(global-set-key (kbd "M-,") 'kill-region) ; cut

(global-set-key (kbd "C-æ") 'undo)

(global-set-key (kbd "M-p") 'delete-backward-char)

(global-set-key (kbd "M-y") 'backward-kill-word)

(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-q q") 'kill-buffer)

(global-set-key (kbd "M-o") 'goto-line)

(global-set-key (kbd "C-q C-.") 'eval-region)

(global-set-key (kbd "C-q C-o") 'save-buffer)
(global-set-key (kbd "C-q C-u") 'find-file)

(global-set-key (kbd "C-q C-j") 'save-buffers-kill-emacs)


;;;; Visual stuff
;; Change default colour theme
(load-theme 'base16-isotope t)
;; Change cursor and line highlighting
(set-default 'cursor-type 'bar)
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t)

;; Disable toolbar
(tool-bar-mode -1)
;; Disable menubar
(menu-bar-mode -1)
;; Disable scroll bar
(scroll-bar-mode -1)
;; Display line numbers
(global-linum-mode t)
;; Disable startup message
(setq inhibit-startup-message t)


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
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-n") 'forward-char)

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
(bind-key* "C-b" 'swindow)

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
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
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

;; Easier-access names
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
