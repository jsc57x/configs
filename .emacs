 
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq makescript "build.bat")
(setq runscript "run.bat")
(setq compilation-directory-locked nil)

;; enable melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

;; disable menubar and toolbar in graphics mode
(menu-bar-mode -1)
(tool-bar-mode -1)

;; sound off
(setq visible-bell t)

;; delete selection on input
(delete-selection-mode +1)

;; set current line highlighted (sadly colides with selection highting)
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

;;  show matching parentheses
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; insert closing parentheses when writing open parenteses
(electric-pair-mode +1)

;; indent new lines according to context
(electric-indent-mode +1)

;; store backups and auto-saved files in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
	  
;; show line numbers and show them relative to the current line
(global-display-line-numbers-mode)
(menu-bar-display-line-numbers-mode 'relative)

;; set custom theme path and load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'doom-peacock t)

;; set custom font and font-size
(add-to-list 'default-frame-alist
                       '(font . "JetBrains Mono NL Regular-12"))

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; activate mini-frame-mode
(mini-frame-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mini-frame-show-parameters '((top . 0.3) (width . 0.7) (left . 0.5) (fullscreen)))
 '(package-selected-packages
   '(mini-frame use-package rainbow-mode magit ido-vertical-mode doom-themes company-irony)))
	 
;; activate ido and ido vertical mode
(require 'ido)
(ido-mode +1)
(require 'ido-vertical-mode)
(ido-vertical-mode +1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only) ;; make C-n and C-p work in ido

;; switch buffers
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "C-c s")  'transpose-windows)
(global-set-key (kbd "C-c C-s")  'transpose-windows)

;; irony configuration
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;; configure company
(use-package company
     :ensure t
     :config
     (setq company-idle-delay 0)
     (global-company-mode t)
)

;; load company-irony after company
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(add-hook 'after-init-hook 'global-company-mode)

;; search selection on google
(defun er-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Google: "))))))
(global-set-key (kbd "C-c g") #'er-google)

;; move current line up
(defun move-line-up()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(control shift up)] 'move-line-up)

(defun move-line-down()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift down)] 'move-line-down)

;; save buffers if focus lost (collides with mini-frame, created issue in github...)
;;(add-hook 'focus-out-hook 'save-buffer)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Compilation
(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p makescript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
;  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile makescript))
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)

; run program
(defun run-in-shell-buffer ()
	"Run the current program"
	(interactive)
	(shell)
	;; Goto last prompt, clear old input if any, and insert new one
	(goto-char (point-max))
	(comint-kill-input)
	(if (find-project-directory) (insert runscript))
	;; Execute
	(comint-send-input))
(define-key global-map "\e," 'run-in-shell-buffer)
	
; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)

; smooth scrolling
(setq scroll-step 3)

; indentation customization
;(setq-default indent-tabs-mode nil)
;(setq-default tab-width 4)
;(setq indent-line-function 'insert-tab)
(setq-default c-basic-offset 4
    tab-width 4
    indent-tabs-mode t)

; always show file name in frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

; close shell buffer without confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
