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


;; Disable startup message
(setq inhibit-startup-message t)


;; Move current line one space up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-<up>") 'move-line-up)

;; Move current line one space down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-<down>") 'move-line-down)
 

;; Ivy does fancy shit
(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-o") 'swiper)


;; Set Ctrl-q == Ctrl-x, for Dvorak practicality
(global-set-key (kbd "C-q") ctl-x-map)

;; Bind "next-buffer" and "previous-buffer" to sensible keys
(global-set-key (kbd "M-å") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)

;; Bind copy, paste and cut to sensible keys
(global-set-key (kbd "C-a") 'yank) ; paste
(global-set-key (kbd "C-,") 'kill-ring-save) ; copy
(global-set-key (kbd "M-,") 'kill-ring) ; cut

;; Bind "undo" to Ctrl-z
(global-set-key (kbd "C-æ") 'undo)

;; Bind "backspace" to Alt-r
(global-set-key (kbd "M-p") 'delete-backward-char)
;; Bind "Ctrl-backspace" to Alt-t
(global-set-key (kbd "M-y") 'backward-kill-word)

;; Bind "kill-buffer" to Ctrl-x x
(global-set-key (kbd "C-q q") 'kill-buffer)

;; Bind "goto-line" to Alt-s
(global-set-key (kbd "M-o") 'goto-line)

;; Bind "comment-region" to Ctrl-c Ctrl-c
(global-set-key (kbd "C-q C-j") 'comment-region)
;; Bind "uncomment-region" To Ctrl-c Ctrl-v
(global-set-key (kbd "C-q C-k") 'uncomment-region)

;; Bind "eval-region" to Ctrl-e Ctrl-r
(global-set-key (kbd "C-q C-.") 'eval-region)

;; Save buffer/file
(global-set-key (kbd "C-q C-o") 'save-buffer)
;; Open file
(global-set-key (kbd "C-q C-u") 'find-file)

;; Change default colour theme
(load-theme 'base16-spacemacs t)
;; Change cursor and line highlighting
(set-default 'cursor-type 'bar)
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t)

;; Disable toolbar
(tool-bar-mode -1)

;; Display line numbers
(global-linum-mode t)

;; Make emacs add matching parenthesis in C-mode
(defun electric-pair () (interactive) (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
(add-hook 'c-mode-hook
	  (lambda ()
	    (define-key c-mode-map "(" 'electric-pair)
	    (define-key c-mode-map "{" 'electric-pair)
	    (define-key c-mode-map "[" 'electric-pair)
	    (define-key c-mode-map "\"" 'electric-pair)))

;; Make emacs add matching parenthesis in C++-mode
(defun electric-pair () (interactive) (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c-mode-map "(" 'electric-pair)
	    (define-key c-mode-map "{" 'electric-pair)
	    (define-key c-mode-map "[" 'electric-pair)
	    (define-key c-mode-map "\"" 'electric-pair)))


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
