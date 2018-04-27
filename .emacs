;;; package --- Summary
;;; Commentary:

;;; Code:

;; ansi term
; edit text mode: C-c C-j
; switch to terminal mode C-c C-k

;; start an emacs server for faster launch
(server-start)

;; Initialize Packages
(setq package-enable-at-startup nil)
(package-initialize)

;; remove top menu bar in mac
(setq ns-auto-hide-menu-bar t)

;; Open .emacs
(defun init ()
  (interactive)
  (find-file "~/.emacs"))

;; Line-Numbers
;;(global-linum-mode 1)
(line-number-mode 1)

;; No Tool-Bar (GUI)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; rainbow-delimiters
(rainbow-delimiters-mode)

;; Color-Theme
;; run M-x fringe-mode to remove fringes from themes
(zerodark-setup-modeline-format)
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'sanityinc-tomorrow-eighties t))

;; neotree customizations
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil)

;; remove the highlighted fringe in most themes
(defun remove-colored-fringe ()
  (interactive)
  (set-face-attribute 'fringe nil :background nil))
;; call function on start-up
(remove-colored-fringe)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Highlight Matching Paren.
(show-paren-mode 1)

;; Tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default typescript-indent-level 2)

;; No-Backups
(setq make-backup-files nil)

;; Column-Number-Mode
(setq column-number-mode t)

;; Font
(set-face-attribute 'default nil :height 115)
;;(set-default-font "Inconsolata")
;;(set-default-font "Menlo")
(set-default-font "Monaco")

;; No Splash-Screen
(setq inhibit-splash-screen t
     initial-scratch-message nil
     initial-major-mode 'text-mode)

;; PATH ENV Variable
(setenv "PATH" (concat (getenv "PATH") "/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(exec-path-from-shell-copy-env "PATH")

;; remove back ups
(setq make-backup-files nil)

;; yes & no = y & n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set Transparency of Emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
     (set-frame-parameter (selected-frame) 'alpha value))
;; Set Transparency at Start-up
(transparency 90)

;; Previous-Terminal-Commmands
(progn (require 'comint)
      (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
      (define-key comint-mode-map (kbd "<down>") 'comint-next-input))

;; Clear-Terminal
(defun my-shell-hook ()
  (local-set-key "\C-d" 'erase-buffer))
(add-hook 'shell-mode-hook 'my-shell-hook)
(put 'erase-buffer 'disabled nil)

;; Return Path of File
(defun path ()
  (interactive)
  (princ buffer-file-name))

;; Return Extension of File
(defun extension ()
  (interactive)
  (print (file-name-extension buffer-file-name)))

; JSON Prettier
(defun json-prettier ()
(interactive)
(save-excursion
  (shell-command-on-region
   (mark) (point) "python -m json.tool"
   (buffer-name) t)))

; Kill all helm buffers
(defun kill-helm ()
  (interactive)
  (kill-matching-buffers "helm"))

;; Org-Mode
(setq org-src-fontify-natively t)

;; Toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; Magit Status short-cut
(global-set-key (kbd "C-x g") 'magit-status)

;; cider
(require 'cider)

;; Load packages
(load-file "~/.emacs.d/.packages.el")

;; Load Programming Lang
(load-file "~/.emacs.d/.programming_lang.el")

;; Load Org-Mode
(load-file "~/.emacs.d/.org.el")

;; Nba scores extension
(load-file "~/.emacs.d/nba-stats.el")

;;; END
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
	 (--map
		(solarized-color-blend it "#002b36" 0.25)
		(quote
		 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
	 (quote
		(("#073642" . 0)
		 ("#546E00" . 20)
		 ("#00736F" . 30)
		 ("#00629D" . 50)
		 ("#7B6000" . 60)
		 ("#8B2C02" . 70)
		 ("#93115C" . 85)
		 ("#073642" . 100))))
 '(hl-bg-colors
	 (quote
		("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
	 (quote
		("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
	 (quote
		("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
	 (quote
		(tuareg zerodark-theme zenburn-theme ycm yaml-mode xterm-color w3m undo-tree ujelly-theme twilight-bright-theme twilight-anti-bright-theme tide telephone-line sublime-themes subatomic256-theme subatomic-theme spacemacs-theme spacegray-theme sourcerer-theme soothe-theme solarized-theme sml-modeline smex smart-mode-line-powerline-theme slack rjsx-mode rebecca-theme rainbow-mode rainbow-delimiters purple-haze-theme projectile pg ox-gfm org-gcal omnisharp obsidian-theme nord-theme nimbus-theme neotree mustang-theme monokai-theme moe-theme meghanada material-theme markdown-mode magit-filenotify lush-theme leuven-theme kaolin-themes json-mode js-comint java-snippets intero imenu-anywhere hide-comnt heroku-theme helm-spotify helm-dash hamburg-theme gruvbox-theme groovy-mode groovy-imports graphql-mode grandshell-theme gradle-mode goto-chg gotham-theme google-maps go-mode gandalf-theme fsharp-mode fringe-helper flymake-json flymake-jshint flymake-cursor flatui-theme flatui-dark-theme flatland-theme finalize exec-path-from-shell epc emojify-logos emacsql-psql elpy dracula-theme doom-themes dockerfile-mode darktooth-theme darcula-theme dante dakrone-theme cyberpunk-theme cuda-mode creamsody-theme company-irony company-emacs-eclim color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern clojure-mode-extra-font-locking cider cherry-blossom-theme calfw-gcal calfw bubbleberry-theme boron-theme base16-theme badwolf-theme badger-theme atom-one-dark-theme atom-dark-theme ample-theme ahungry-theme afternoon-theme ac-helm abyss-theme)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#dc322f")
		 (40 . "#c85d17")
		 (60 . "#be730b")
		 (80 . "#b58900")
		 (100 . "#a58e00")
		 (120 . "#9d9100")
		 (140 . "#959300")
		 (160 . "#8d9600")
		 (180 . "#859900")
		 (200 . "#669b32")
		 (220 . "#579d4c")
		 (240 . "#489e65")
		 (260 . "#399f7e")
		 (280 . "#2aa198")
		 (300 . "#2898af")
		 (320 . "#2793ba")
		 (340 . "#268fc6")
		 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
	 ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
	 ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
