(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package modern-cpp-font-lock
  :ensure t)
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 1.5)))))))

(setq aquamacs (featurep 'aquamacs))
(setq linux (featurep 'x))
(setq win32 (not (or aquamacs linux)))
(when win32
  (set-face-attribute 'default nil :font "Consolas-11")
  (setq makescript "build.bat"))
(when aquamacs
  (setq makescript "./build.macosx"))
(when linux
  (setq makescript "./build.sh")
  (show-paren-mode 1)
  (setq default-directory "~/")
  (setq ring-bell-function #'ignore))

(setq inhibit-splash-screen t)
(setq visible-bell 1)
(setq auto-revert-verbose nil)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))
(global-auto-revert-mode 1)

;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

(setq-default truncate-lines t)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq scroll-step 1)
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq c-default-style "linux"
      c-basic-offset 4)

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs-backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq backup-directory-alist backup-directory-alist)
(setq auto-save-directory autosave-dir)

(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))

(defun dotemacs ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))

(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))
(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))
(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))
(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))
(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))
(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))
(global-set-key [C-M-down] 'win-resize-minimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun swap-windows ()
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

(defun rename-file-and-buffer (new-name)
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn (rename-file filename new-name 1)
	       (rename-buffer new-name)
	       (set-visited-file-name new-name)
	       (set-buffer-modified-p nil))))))
(defun move-buffer-file (dir)
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn  (copy-file filename newname 1)
	      (delete-file filename)
	      (set-visited-file-name newname)
	      (set-buffer-modified-p nil) t))))

(defun back-to-indentation-or-beginning ()
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (progn (move-beginning-of-line nil)
                (when (= (line-beginning-position) (line-end-position))
                  (save-excursion (indent-according-to-mode)))))))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))
(defun move-text-down (arg)
  (interactive "*p")
  (move-text-internal arg))
(defun move-text-up (arg)
  (interactive "*p")
  (move-text-internal (- arg)))
(provide 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

(defun find-project-directory ()
  (defun find-project-directory-recursive ()
    (interactive)
    (if (file-exists-p makescript) t
      (cd "../")
      (find-project-directory-recursive)))
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory))
(defun make-build()
  (interactive)
  (if (find-project-directory)
      (compile makescript))
  (other-window 1))
(global-set-key [f5] 'make-build)

(defvar install-theme-loading-times nil
  "An association list of time strings and theme names.
The themes will be loaded at the specified time every day.")
(defvar install-theme-timers nil)
(defun install-theme-loading-at-times ()
  "Set up theme loading according to `install-theme-loading-at-times`"
  (interactive)
  (dolist (timer install-theme-timers)
    (cancel-timer timer))
  (setq install-theme-timers nil)
  (dolist (time-theme install-theme-loading-times)
    (add-to-list 'install-theme-timers
		 (run-at-time (car time-theme) (* 60 60 24) 'load-theme (cdr time-theme)))))

(defvar theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path theme-load-path)
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "light blue")
(setq install-theme-loading-times '(("9:00am" . moe-light)
				    ("8:00pm" . moe-dark)))
(install-theme-loading-at-times)

(defvar font-lock-modes '(c++-mode c-mode glsl-mode emacs-lisp-mode lisp-mode))
(make-face 'font-lock-todo-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	   ("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t))))
      font-lock-modes)
(modify-face 'font-lock-todo-face "red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "green" nil nil t nil t nil nil)
(modify-face 'font-lock-fixme-face "slate blue" nil nil t nil t nil nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("76bebbc3c8938de48fd50881a2692bc29c022d08f27555202a079c407f255609" "7df8a91fed857487fcb79868e44247ed92a8c58ea0bbd3fe0f3da1f9370f2c7f" default))
 '(ispell-dictionary nil)
 '(package-selected-packages '(moe-theme ace-window glsl-mode which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 1.5)))))
