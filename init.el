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
(global-set-key (kbd "C-s") 'swiper)

;; Bind "next-buffer" and "previous-buffer" to sensible keys
(global-set-key (kbd "M-q") 'previous-buffer)
(global-set-key (kbd "M-e") 'next-buffer)

;; Bind "undo" to Ctrl-z
(global-set-key (kbd "C-z") 'advertised-undo)

;; Bind "backspace" to Alt-r
(global-set-key (kbd "M-r") 'delete-backward-char)
;; Bind "Ctrl-backspace" to Alt-t
(global-set-key (kbd "M-t") 'backward-kill-word)

;; Bind "kill-buffer" to Ctrl-x x
(global-set-key (kbd "C-x x") 'kill-buffer)

;; Bind "goto-line" to Alt-s
(global-set-key (kbd "M-s") 'goto-line)

;; Bind "comment-region" to Ctrl-c Ctrl-c
(global-set-key (kbd "C-c C-c") 'comment-region)
;; Bind "uncomment-region" To Ctrl-c Ctrl-v
(global-set-key (kbd "C-c C-v") 'uncomment-region)

;; Change default font colour
(add-to-list 'default-frame-alist '(foreground-color . "#8F8F8F"))

;; Change default background colour
(add-to-list 'default-frame-alist '(background-color . "#222222"))

;; Disable toolbar
(tool-bar-mode -1)

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
(global-set-key (kbd "C-x C-r") 'restart-emacs)


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


;; Unholy abomination which should make Ctrl-backspace fancy
;(defun aborn/backward-kill-word ()
;  "Customize/Smart backward-kill-word."
;  (interactive)
;  (let* ((cp (point))
;         (backword)
;         (end)
;         (space-pos)
;         (backword-char (if (bobp)
;                            ""           ;; cursor in begin of buffer
;                          (buffer-substring cp (- cp 1)))))
;    (if (equal (length backword-char) (string-width backword-char))
;        (progn
;          (save-excursion
;            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
;          (setq ab/debug backword)
;          (save-excursion
;            (when (and backword          ;; when backword contains space
;                       (s-contains? " " backword))
;              (setq space-pos (ignore-errors (search-backward " ")))))
;          (save-excursion
;            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
;                   (substr (when pos (buffer-substring pos cp))))
;              (when (or (and substr (s-blank? (s-trim substr)))
;                        (s-contains? "\n" backword))
;                (setq end pos))))
;          (if end
;              (kill-region cp end)
;            (if space-pos
;                (kill-region cp space-pos)
;              (backward-kill-word 1))))
;      (kill-region cp (- cp 1)))         ;; word is non-english word
;    ))

;(global-set-key  [C-backspace]
;            'aborn/backward-kill-word)
