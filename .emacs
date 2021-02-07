
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; enable melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

;; activate ido and ido vertical mode
(require 'ido)
(ido-mode +1)
(require 'ido-vertical-mode)
(ido-vertical-mode +1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only) ;; make C-n and C-p work in ido

;; activate mini-frame-mode
(mini-frame-mode 1)
;; configure mini-frame
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "7923541211298e4fd1db76c388b1d2cb10f6a5c853c3da9b9c46a02b7f78c882" "f58715f68262936e4e5f6995aa82d0fb4250ed18bfa83c17ccf2b33ce1cef611" "7510c9c175f736d50b3c6589380c460b66b86762a17b728f13b18013188d2385" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" default))
 '(frame-brackground-mode 'dark)
 '(global-display-line-numbers-mode t)
 '(line-number-mode nil)
 '(mini-frame-show-parameters '((top . 0.3) (width . 0.5) (left . 0.5)))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(use-package mini-frame treemacs doom-themes zenburn-theme zerodark-theme chess rainbow-mode magit gruber-darker-theme cyberpunk-theme))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838")))

;; disable menubar and toolbar in graphics mode
(menu-bar-mode -1)
(tool-bar-mode -1)

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

;; save buffers if focus lost (collides with mini-frame, created issue in github...)
;;(add-hook 'focus-out-hook 'save-buffer)

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

;; sound off
(setq visible-bell t)

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

;;
;; Custom functions
;;

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



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
