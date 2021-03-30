;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; For native compilation
(setq comp-speed 2)

(when (boundp 'comp-eln-load-path)
  (let ((eln-cache-dir (expand-file-name "eln-cache/" user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete"))))

(if (eq system-type 'windows-nt)
    (load-file (concat doom-private-dir "windows.el")))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sean Wenzel")

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Revert (update) buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq-default
 ;; General Defaults
 confirm-kill-emacs nil                 ; Don't have to confirm to kill emacs
 confirm-kill-processes nil             ; Don't have to confirm to kill running processes
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 delete-by-moving-to-trash nil          ; Delete files without moving to trash
 fill-column 80                         ; Set width for automatic line breaks
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-message t              ; Don't show the startup message
 inhibit-startup-screen t               ; inhibit useless and old-school startup screen
 initial-scratch-message nil            ; Empty scratch buffer
 line-spacing nil                       ; I sometimes like some line spacing
 ring-bell-function 'ignore             ; silent bell when you make a mistake
 sentence-end-double-space nil          ; End a sentence after a dot and a space
 show-trailing-whitespace nil           ; Display trailing whitespaces
 tab-width 4                            ; Set width for tabs
 x-stretch-cursor t                     ; Stretch cursor to the width of the underlying glyph

 ;; Backup File Defaults
 auto-save-default nil                  ; stop creating #autosave# files
 create-lockfiles nil                   ; stop creating .# files
 make-backup-files nil                  ; stop creating backup~ files
 )

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-pro)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(global-subword-mode t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package! evil
  :custom
  evil-disable-insert-state-bindings t)

;; Very Large Files Mode lazy loading
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; This shows duplicate snippets for some reason, even though company-yasnippet doesn't
(use-package ivy-yasnippet
  :config
  (map! :leader :desc "Snippet" "i s" #'ivy-yasnippet))

(after! company
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2)
  ;; (setq company-show-numbers t) ;; Select a selection with M-number
  ;; (add-hook 'shell-mode-hook ('company-mode -1))
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(after! org
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(after! projectile
  :config
  (setq projectile-project-search-path '("~/projects/" "~/repositories/")))

;; Keybindings
(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

;; Swap : and ;
(map! :nvm ";" 'evil-ex
      :nvm ":" 'evil-snipe-repeat)

(map! :leader
      (:prefix-map ("g" . "git")
       :desc "Magit status" "s" #'magit-status))

(setq avy-all-windows 'all-frames)
(map! :leader
      (:prefix-map ("j" . "jump")
       :desc "Jump to line" "l" #'evil-avy-goto-line
       :desc "Jump to char" "j" #'evil-avy-goto-char
       :desc "Jump to char 2" "s" #'evil-avy-goto-char-2
       :desc "Jump to word" "w" #'evil-avy-goto-word-or-subword-1))

(use-package! magit
  :bind ((("C-c g" . magit-file-dispatch))))

                                        ; Hybrid doesn't ignore directories using WSL
;; (setq projectile-indexing-method 'native)

;; Interpret ansi escape sequences in log files
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
