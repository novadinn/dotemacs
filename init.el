(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package try :ensure t)
(use-package which-key :ensure t :config (which-key-mode))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package modern-cpp-font-lock
  :ensure t)

;; stops startup message
(setq inhibit-splash-screen t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq default-directory "D:/Projects/")
(setq visible-bell 1)
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 90))

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs-backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

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

(setq c-default-style "linux"
      c-basic-offset 4)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default truncate-lines t)
;; replace highlighted text with a new text
(delete-selection-mode 1)
(setq compile-command "build.bat")
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "light blue")
(setq scroll-step 1)
(setq confirm-kill-emacs 'y-or-n-p)
(global-set-key [f5] 'compile)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'vim-colors t)

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Green" nil nil t nil t nil nil)
(set-face-attribute 'default nil :font "Consolas-11")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
