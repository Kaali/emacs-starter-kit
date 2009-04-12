;;
;; Set up some variables
;;
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))


;;
;; Set my PATH envvar (broke with Emacs 23)
;;
(setenv "PATH" (concat "/usr/local/bin:"
                       "/opt/local/bin:"
                       "/usr/texbin:"
                       (concat (getenv "HOME") "/bin:")
                       (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin"
                                    "/opt/local/bin"
                                    "/usr/texbin:"
                                    (concat (getenv "HOME") "/bin"))))

;;
;; Set my theme
;;
(zenburn)
(setq background-mode 'dark)
(setq frame-background-mode 'dark)
(set-face-font 'default "-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1")


;; Store 3rd-party libraries  in "vendor/".
(setq vendor-dir (concat dotfiles-dir "vendor/"))
(add-to-list 'load-path vendor-dir)
(progn (cd vendor-dir)
       (normal-top-level-add-subdirs-to-load-path))

;;
;; Package setup
;;

;; Flymake-mode

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))
(define-key global-map (kbd "C-c ;") 'credmp/flymake-display-err-minibuf)


(require 'flymake)
(set-face-attribute 'flymake-warnline nil
                    :background zenburn-bg
                    :underline "#ff7700")
(set-face-attribute 'flymake-errline nil
                    :background zenburn-bg
                    :underline "#ff0000")

(setq flymake-allowed-file-name-masks
      '(("\\.c\\'" flymake-simple-make-init)
        ("\\.cpp\\'" flymake-simple-make-init)
        ("\\.cs\\'" flymake-simple-make-init)
        ("\\.pl\\'" flymake-perl-init)
        ("\\.h\\'" flymake-master-make-header-init
         flymake-master-cleanup)
        ("\\.java\\'" flymake-simple-make-java-init
         flymake-simple-java-cleanup)
        ("[0-9]+\\.tex\\'" flymake-master-tex-init
         flymake-master-cleanup)
        ("\\.tex\\'" flymake-simple-tex-init)
        ("\\.idl\\'" flymake-simple-make-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-get-tex-args (file-name)
  (list "/usr/texbin/latex" (list "-file-line-error-style" file-name)))

;; Flyspell-mode
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'rst-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
;; Disabled because of problems with unicode strings
;; (add-hook 'python-mode-hook 'flyspell-prog-mode)
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; EDiff setup
;; I don't want any new frames with ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;; Org-mode
;; Remove org-mode meta-cursor bindings as it's used by windmove
(add-hook 'org-mode-hook
          '(lambda ()
             (local-unset-key [(meta down)])
             (local-unset-key [(meta up)])
             (local-unset-key [(meta left)])
             (local-unset-key [(meta right)])))

;; Ido-mode
(ido-mode nil)
;; (ido-everywhere t)
;; Use Ido for M-x command completion
;; (setq ido-execute-command-cache nil)
;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-command-cache
;; 	 (mapatoms (lambda (s)
;; 		     (when (commandp s)
;; 		       (setq ido-execute-command-cache
;; 			     (cons (format "%S" s) ido-execute-command-cache))))))
;;        ido-execute-command-cache)))))
    
;; (global-set-key "\M-x" 'ido-execute-command)

;; Winner-mode
(winner-mode t)

;; ERC
(eval-after-load "erc"
                 '(progn
                    (setq erc-pals '("Raekkeri" "ArtemD" "misantrooppi" "MaeK" "mts")
                          erc-notify-list erc-pals)
                    (require 'erc-notify)
                    (erc-completion-mode t)
                    (erc-match-mode t)
                    (erc-netsplit-mode t)
                    (erc-timestamp-mode t)
                    (erc-track-mode t)))


;; Tabkey2
(tabkey2-mode t)
(setq tabkey2-message-style 'echo-area)


;; Pymacs / rope
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)


;; Yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/snippets")
(yas/load-directory "~/.emacs.d/snippets")


;; Whitespace-mode
(require 'whitespace)
(set-face-attribute 'whitespace-tab nil
                    :foreground zenburn-bg+1
                    :background zenburn-bg)
(set-face-attribute 'whitespace-indentation nil
                    :foreground zenburn-yellow
                    :background zenburn-yellow-2)
(set-face-attribute 'whitespace-trailing nil
                    :foreground zenburn-red
                    :background zenburn-red-3)
(setq whitespace-style
      '(tabs trailing lines space-before-tab newline
             indentation space-after-tab tab-mark))
(setq whitespace-global-modes t)
(global-whitespace-mode t)


;; Haskell-mode
(load (concat vendor-dir "haskell-mode-2.4/haskell-site-file"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;;
;; Key bindings / input
;;

;; Mac key settings
(setq mac-pass-option-to-system t)
(setq mac-command-key-is-meta t)
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'control)
(setq mac-pass-command-to-system nil)

;; Move between windows/frames with meta+arrows
(windmove-default-keybindings 'meta)

;; Sane mouse wheel behaviour
(setq mouse-wheel-progressive-speed nil)

;; Always reindent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Fullscreen toggling  on Carbon Emacs
(defun toggle-fullscreen () 
  (interactive) 
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                           nil 
                                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; Vim like open line
(defun vj/open-line-after ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'vj/open-line-after)

;; Zap up to char like in Vim, Emacs behaviour maps to M-Z
(defun vj/zap-up-to-char (arg char)
  "Zap up to a character."
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (forward-char -1))
(global-set-key (kbd "M-z") 'vj/zap-up-to-char)
(global-set-key (kbd "M-Z") 'vj/zap-to-char)



;;
;; Autoload file modes
;;

;; Restructured Text 
(require 'rst)
(autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)


;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;
;; Text mode settings
;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;
;; Python
;;

;; Use IPython
;(require 'ipython)

;; Set Python indentation to 4 spaces
(defun vj/python-hook ()
  (setq tab-width 4)
  (setq python-indent 4))
(add-hook 'python-mode-hook 'vj/python-hook)

;; Flymake Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat vendor-dir "epylint") (list local-file))))
    
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;;
;; Icicles
;;
(add-to-list 'load-path (concat vendor-dir "icicles"))
(require 'icicles)
(icy-mode t)
(global-set-key (kbd "C-x f") 'icicle-recent-file)
(global-set-key "\C-x\C-i" 'imenu)
(global-set-key (kbd "C-x M-f") 'find-file-other-window)


;;
;; C / C++
;;

;; Set my default C style
(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk") (other . "bsd"))
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

;;
;; Miscellaneous
;;

;; Automatically set execute perms on files if first line begins with '#!'
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Change cutting behaviour:
;;  "Many times you'll do a kill-line command with the only intention of
;;  getting the contents of the line into the killring. Here's an idea
;;  stolen from Slickedit, if you press copy or cut when no region is
;;  active you'll copy or cut the current line:"
;;  <http://www.zafar.se/bkz/Articles/EmacsTips>
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't diable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Build etags for a directory
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "cd %s; find . -type f | xargs -n1 -P2 %s > TAGS"
           dir-name (concat vendor-dir "etags.sh"))))

;; Mark-ring is navigable by typing C-u C-SPC and then repeating C-SPC forever
(setq set-mark-command-repeat-pop t)


;; From http://hjiang.net/archives/253, optional arg added by
;; Väinö Järvelä
(defun smart-split (&optional arg)
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns.

   If OPTIONAL arg is supplied, then use that as the minimum width of the
   windows."
  (interactive "P")
  (defun smart-split-helper (w min-width)
    "Helper function to split a given window into two, the first of which
     has min-width columns."
    (if (> (window-width w) (* 2 (+ min-width 1)))
    (let ((w2 (split-window w (+ min-width 2) t)))
      (smart-split-helper w2 min-width))))
  (smart-split-helper nil (or arg 80)))

(define-key global-map (kbd "C-c s") 'smart-split)


;; Some settings
(column-number-mode t)
(cua-mode t)
(setq cua-enable-cua-keys nil)

(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq transient-mark-mode t)
(show-paren-mode t)
(size-indication-mode t)
(setq-default fill-column 78)
(setq comment-style 'indent)

;; Confirm killing, because of accidental kills when in org-mode
(setq confirm-kill-emacs 'yes-or-no-p)

;; Follow git commit message best practices in Magit.
(add-hook 'magit-log-edit-mode-hook (lambda ()
                                      (setq fill-column 72)
                                      (turn-on-auto-fill)))

;; Use Safari as my browser through Applescript
(setq browse-url-browser-function
    (lambda (url &optional new-window) 
      (message url)
      (do-applescript (concat "tell application \"Safari\" \n activate \n open location \"" url "\" \n end tell"))))