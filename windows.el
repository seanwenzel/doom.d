;;; windows.el -*- lexical-binding: t; -*-

;; Right now this uses Git Bash. Could also use WSL
(defun run-bash ()
      (interactive)
      (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
            (shell "*bash*")))

(defun run-cmdexe ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
            (shell "*cmd.exe*")))

(load-file (concat doom-private-dir "powershell.el"))
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)

;; (setq ahk-syntax-directory "PATHTO/AutoHotkey/Extras/Editors/Syntax/")
(load-file (concat doom-private-dir "ahk-mode.el"))
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(autoload 'ahk-mode "ahk-mode")

;; Windows performance tweaks for irony-mode
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(cd "~/")
