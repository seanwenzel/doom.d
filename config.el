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
    (load-file (concat doom-private-dir "windows.el"))
    )

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sean Wenzel")

(setq-default
 delete-by-moving-to-trash t ; Delete files to trash
 tab-width 4 ; Set width for tabs
 x-stretch-cursor t ; Stretch cursor to the glyph width
 )

(setq-default history-length 1000)

(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular.
      truncate-string-ellipsis "…"      ; Unicode ellipsis are nicer than "..." and saves space
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
(setq doom-theme 'my-monokai-pro)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/"))

(setq projectile-project-search-path '("~/projects/" "~/repositories/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(global-subword-mode t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq +ivy-buffer-preview t)

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

(use-package evil
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

;; Keybindings
(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

;; Swap : and ;
(map! :nvm ";" 'evil-ex
      :nvm ":" 'evil-snipe-repeat)

(map! :after import-js
      :map js2-mode-map
      :localleader :desc "import-js" "i" #'import-js-import)

(map! :leader
      (:prefix-map ("g" . "git")
        :desc "Magit status" "s" #'magit-status))

(setq avy-all-windows 'all-frames)
(map! :leader
      (:prefix-map ("j" . "jump")
       :desc "Jump to line" "l" #'evil-avy-goto-line
       :desc "Jump to char" "j" #'evil-avy-goto-char
       :desc "Jump to char 2" "J" #'evil-avy-goto-char-2
       :desc "Jump to word" "w" #'evil-avy-goto-word-or-subword-1))


;; This may not be necessary since Doom already has an analogue for this functionality
(setq org-projectile-projects-directory "~/org/projects/")
(use-package! org-projectile
  :config
  (org-projectile-per-project)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (map! :leader
        (:prefix "n"
         :desc "projectile-project-complete-read" "p" #'org-projectile-project-todo-completing-read)))

(use-package! magit
  :bind ((("C-c g" . magit-file-dispatch))))

; Hybrid doesn't ignore directories using WSL
(setq projectile-indexing-method 'native)

(setq-hook! 'js2-mode-hook +format-with-lsp nil)
