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
;; Scala stuff
(use-package ensime
 :ensure t
 :pin melpa-stable)
;; Allow unmarked use of region commands, e.g. copy single line
(use-package whole-line-or-region
  :ensure t
  :defer t)
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
      ("C-s" 'forward-char)
      ("C-v" 'move-beginning-of-line)
      ("C-z" 'move-end-of-line)
      ("C-M-<left>" 'shrink-window-horizontally)
      ("C-M-<right>" 'enlarge-window-horizontally)
      ))


;;;; Convenience and quality of life, random stuff in general
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
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-function-calls-mode)


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


;;;; omfg CUDA mode
;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))


(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'cuda-mode 'c++-mode))

;; cuda has no boolean but a string and a vector type.
(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  cuda 
  (append 
   '("dim3" 
	 "char1" "uchar1" "char2" "uchar2" "char3" "uchar3" "char4" "uchar4"
	 "short1" "ushort1" "short2" "ushort2" "short3" "ushort3" "short4" "ushort4"
	 "int1" "uint1" "int2" "uint2" "int3" "uint3" "int4" "uint4"
	 "long1" "ulong1" "long2" "ulong2" "long3" "ulong3" "long4" "ulong4"
	 "float1" "float2"  "float3" "float4" 
	 "double1" "double2" )
   ;; Use append to not be destructive on the
   ;; return value below.
   (append
	(c-lang-const c-primitive-type-kwds)
	nil)))

(c-lang-defconst c-type-modifier-keywds
  "Type modifier keywords.  These can occur almost anywhere in types
but they don't build a type of themselves.  Unlike the keywords on
`c-primitive-type-kwds', they are fontified with the keyword face and
not the type face."
  cuda
    (append 
      '("__device__", "__global__", "__shared__", "__host__", "__constant__") 
      (c-lang-const c-type-modifier-keywds) 
      nil))

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
  cuda
  (append '("#" "##"	; Used by cpp.
	    "::" "..." "<<<" ">>>")
	  (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  cuda  '("gridDim" "blockIdx" "blockDim" "threadIdx" "warpSize"))

(c-lang-defconst c-paren-nontype-kwds
  "Keywords that may be followed by a parenthesis expression that doesn't
contain type identifiers."
  cuda       nil
  (c c++) '(;; GCC extension.
	    "__attribute__"
	    ;; MSVC extension.
	    "__declspec"))

(defcustom cuda-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in Cuda mode.
Each list item should be a regexp matching a single identifier.")

(defconst cuda-font-lock-keywords-1 
  (c-lang-const c-matchers-1 cuda)
  "Minimal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-2 
  (c-lang-const c-matchers-2 cuda)
  "Fast normal highlighting for CUDA mode.")

(defconst cuda-font-lock-keywords-3 
  (c-lang-const c-matchers-3 cuda)
  "Accurate normal highlighting for CUDA mode.")

(defvar cuda-font-lock-keywords cuda-font-lock-keywords-3
  "Default expressions to highlight in CUDA mode.")

(defvar cuda-mode-syntax-table nil
  "Syntax table used in cuda-mode buffers.")
(or cuda-mode-syntax-table
    (setq cuda-mode-syntax-table
      (funcall (c-lang-const c-make-mode-syntax-table cuda))))

(defvar cuda-mode-abbrev-table nil
  "Abbreviation table used in cuda-mode buffers.")

(c-define-abbrev-table 'cuda-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar cuda-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for CUDA
              map)
  "Keymap used in cuda-mode buffers.")

(easy-menu-define cuda-menu cuda-mode-map "CUDA Mode Commands"
          ;; Can use `cuda' as the language for `c-mode-menu'
          ;; since its definition covers any language.  In
          ;; this case the language is used to adapt to the
          ;; nonexistence of a cpp pass and thus removing some
          ;; irrelevant menu alternatives.
          (cons "CUDA" (c-lang-const c-mode-menu cuda)))

;;;###Autoload
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;;;###autoload
(defun cuda-mode ()
  "Major mode for editing CUDA Cuda is a C like language extension
for mixed native/GPU coding created by NVIDA
 
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `cuda-mode-hook'.

Key bindings:
\\{cuda-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table cuda-mode-syntax-table)
  (setq major-mode 'cuda-mode
    mode-name "Cuda"
    local-abbrev-table cuda-mode-abbrev-table
    abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars cuda-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'cuda-mode)
  (easy-menu-add cuda-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'cuda-mode-hook)
  (setq font-lock-keywords-case-fold-search t)
  (c-update-modeline))

 
(provide 'cuda-mode)
