;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sean Wenzel")

(setq-default
 delete-by-moving-to-trash t ; Delete files to trash
 tab-width 4 ; Set width for tabs
 x-stretch-cursor t ; Stretch cursor to the glyph width
 )

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
(setq doom-font (font-spec :family "Source Code Pro" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq projectile-project-search-path '("~/projects/" "~/repositories/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(global-subword-mode t)


(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Prompt for buffer selection on window split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

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

(setq doom-localleader-key ",")

;; This shows duplicate snippets for some reason, even though company-yasnippet doesn't
(use-package ivy-yasnippet
  :config
  (map! :leader :desc "Snippet" "i s" #'ivy-yasnippet))

(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

;; Swap : and ;
(map! :nvm ";" 'evil-ex
      :nvm ":" 'evil-snipe-repeat)
