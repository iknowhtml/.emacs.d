 ;;
;;configures package manager
;;
(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    auto-complete
    tern-auto-complete
    yasnippet
    neotree
    web-mode
    js2-mode
    move-text
    ) "a list of packages to ensure are installed at launch.")


; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t))
  )

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;
;;enables and configures web-mode
;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("html" . "\\.js[x]?\\'"))) 

;;
;;enables and configures js2-mode
;;
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-highlight-level 3)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>");

;;
;;configures tern
;;
(add-to-list 'load-path "/usr/local/bin/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
;sets tern-mode to js mode of choice
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;use auto-complete for completions
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
; command to kill tern tern server
 (defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))(tern-ac-setup)))

;;
;;configures move-text
;;
(require 'move-text)
((message "message" format-args)ove-text-default-bindings)

;;
;;configures neotree
;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;
;;Don't make backup files 
;;
(setq make-backup-files nil)

;;
;;inhibits start menu
;;
(setq inhibit-startup-message t)

;;
;;enables numbered lines 
;;
(global-linum-mode t)

;;
;;enables copy paste w/ ctrl+c/v & undo w/ ctrl+z
;;
(setq x-select-enable-clipboard t)
(cua-mode t)

;;
;;custom keyboard command for switching windows 
;;
(windmove-default-keybindings)

;;
;;disables the menu bar
;;
(menu-bar-mode -1) 

;;
;;disable scrollbar
;;
(toggle-scroll-bar -1) 

;;
;;disable the toolbar
;;
(tool-bar-mode -1) 

;;
;;set theme
;;
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces)

