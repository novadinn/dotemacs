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
(when linux
  (show-paren-mode 1)
  (setq default-directory "~/")
  (setq ring-bell-function #'ignore))

(setq inhibit-splash-screen t)
(setq visible-bell 1)
(setq auto-revert-verbose nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))
(global-auto-revert-mode 1)

(setq-default truncate-lines t)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq scroll-step 1)
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs-backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq backup-directory-alist backup-directory-alist)
(setq auto-save-directory autosave-dir)

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

(defvar theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path theme-load-path)
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "light blue")
(defvar *dark-theme* 'doom-ir-black)
(defvar *light-theme* 'doom-acario-light)
(defvar current-theme *dark-theme*)
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))
(defun next-theme (theme)
  (if (eq theme 'default)
      (disable-theme current-theme)
    (progn
      (load-theme theme t)))
  (setq current-theme theme))
(defun toggle-theme ()
  (interactive)
  (cond ((eq current-theme *dark-theme*) (next-theme *light-theme*))
        ((eq current-theme *light-theme*) (next-theme *dark-theme*))))
(next-theme current-theme)

(defvar font-lock-modes '(c++-mode c-mode csharp-mode glsl-mode emacs-lisp-mode lisp-mode))
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
