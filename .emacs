;;; package --- Summary
;;; Commentary:

;;; Code:

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
(global-linum-mode 1)

;; No Tool-Bar (GUI)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; rainbow-delimiters
(rainbow-delimiters-mode)

;; Color-Theme
;; run M-x fringe-mode to remove fringes from themes
(if window-system
    (load-theme 'dracula t)
  (load-theme 'wombat t))

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Highlight Matching Paren.
(show-paren-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

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
(progn(require 'comint)
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(eclim-eclipse-dirs (quote ("~/java-neon/eclipse/")))
 '(eclim-executable "~/java-neon/eclipse/eclim")
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (intero dante json-mode darcula-theme rjsx-mode graphql-mode slack meghanada zenburn-theme ycm yaml-mode xterm-color w3m undo-tree ujelly-theme twilight-bright-theme twilight-anti-bright-theme sublime-themes subatomic256-theme subatomic-theme spacemacs-theme sourcerer-theme soothe-theme solarized-theme sml-modeline smex smart-mode-line-powerline-theme rainbow-mode rainbow-delimiters purple-haze-theme projectile ox-gfm org-gcal omnisharp neotree mustang-theme monokai-theme moe-theme material-theme markdown-mode magit-filenotify leuven-theme js2-mode js-comint java-snippets imenu-anywhere hide-comnt helm-spotify helm-dash gruvbox-theme groovy-mode groovy-imports grandshell-theme gradle-mode goto-chg gotham-theme google-maps fsharp-mode fringe-helper flymake-json flymake-jshint flymake-cursor flatui-theme flatui-dark-theme exec-path-from-shell epc emacsql-psql elpy dracula-theme doom-themes darktooth-theme cyberpunk-theme cuda-mode creamsody-theme company-irony company-emacs-eclim color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern clojure-mode-extra-font-locking cider calfw-gcal calfw base16-theme badwolf-theme badger-theme atom-one-dark-theme atom-dark-theme ample-theme ahungry-theme afternoon-theme ac-helm abyss-theme)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
