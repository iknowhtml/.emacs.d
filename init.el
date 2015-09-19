;;
;;Sets custom foreground and background colors
;;
;;(custom-set-faces  ;;  only one 'custom-set-faces' entry may exist in .emacs!!
;;'(default ((t (:foreground "white" :background "black" :bold t))) t)
;; '(default ((t (:foreground "white" :background "black" t))) t)
;;'(isearch ((t (:foreground "black" :background "yellow"))) t)
;;)

;;
;;Don't make backup files 
;;
(setq make-backup-files nil)

;;
;;Sets up emacs packages 
;;
 
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
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
