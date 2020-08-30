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

(load-file "powershell.el")
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)
