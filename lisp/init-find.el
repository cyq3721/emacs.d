;;; package --- Summary
;;; Commentary:
;;; Code:
;;配置ivy
(require-package 'counsel)
(use-package ivy 
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c C-f" . 'counsel-find-file)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))
;;---------------------------------------------
;;开启amx，这个插件可以记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面，这对于我们短时间希望频繁输入某几个命令的场景非常有用。
(use-package amx
  :ensure t
  :init
  (amx-mode))

;;-------------开启ace-window
(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

;;-----开启mwim
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-gogo-word-1)
  ("M-g e" . avy-goto-word-0))

(provide 'init-find)
;;; init-find.el ends here
