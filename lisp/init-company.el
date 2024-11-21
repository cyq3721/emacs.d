;;; package --- Summary
;;; Commentary:
;;; code:
(require-package 'company)
(require-package 'emmet-mode)
(require-package 'web-mode)
(require-package 'company-web)
(require-package 'company-quickhelp)
(require-package 'ac-html-csswatcher)
(eval-after-load 'company-web '(ac-html-csswatcher-setup))

(require-package 'web-mode-edit-element)
(add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode)

;;(require-package 'flycheck )
;;(global-flycheck-mode t)
(require-package 'yasnippet)
(company-quickhelp-mode)
(require-package 'company-tabnine)
(global-company-mode 1)

(add-to-list 'company-backends #'company-tabnine)
(add-hook 'after-init-hook 'global-company-mode)  ;;全局启用company-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook 'web-mode)

(eval-after-load 'company
   '(define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin))

(yas-global-mode 1)
(add-hook 'after-init-hook 'global-company-mode)          ;;开启补全
(setq company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers 'all
        company-require-match nil
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay .3
        company-tooltip-offset-display 'scrollbar
        company-begin-commands '(self-insert-command))
(push '(company-semantic :with company-yasnippet) company-backends)


 ;;company for html
(require 'company-web-html)
(require 'company-web-jade)
(require 'company-web-slim)

(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)

;; you may key bind, for example for web-mode:
(eval-after-load 'web-mode  
      '(define-key web-mode-map (kbd "C-'") 'company-web-html))

(setq company-minimum-prefix-length 2)            ; WARNING, probably you will get perfomance issue if min len is 0!
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start
;;autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete
                                        ; file names on "C-c /" key

(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))


;;Color customization

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))


(defun my-web-mode-hook ()
  "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tern company-web-html company-yasnippet company-files)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Enable JavaScript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                          (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))

;; manual autocomplete
(eval-after-load 'web-mode
     '(define-key web-mode-map (kbd "M-SPC") 'company-complete))

;;company for php
(require-package 'php-mode)
;;(require 'web-mode)
(add-hook 'web-mode-hook
          '(lambda ()
             ;; Enable company-mode
             (company-mode t)
             
             (require-package 'company-php)
             (require-package 'ac-php)
             (setq ac-sources '(ac-source-php))

             ;; As an example (optional)
             (yas-global-mode 1)


             ;; Enable ElDoc support (optional)
             (ac-php-core-eldoc-setup)

             (set (make-local-variable 'company-backends)
                  '((company-ac-php-backend company-dabbrev-code)
                    company-capf company-files))

             ;; Jump to definition (optional)
             (define-key php-mode-map (kbd "M-]")
               'ac-php-find-symbol-at-point)

             ;; Return back (optional)
             (define-key php-mode-map (kbd "M-[")
               'ac-php-location-stack-back)))

(add-hook 'web-mode-hook '(company-mode t))

(provide 'init-company)
;;; init-company.el ends here
