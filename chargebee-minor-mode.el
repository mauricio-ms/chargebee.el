(make-variable-buffer-local
  (defvar chargebee-minor-mode nil
    "Toggle chargebee-minor-mode."))

(defvar chargebee-minor-mode-map (make-sparse-keymap)
  "The keymap for chargebee-minor-mode")

(add-to-list 'minor-mode-alist '(chargebee-minor-mode " chargebee"))
(add-to-list 'minor-mode-map-alist (cons 'chargebee-minor-mode chargebee-minor-mode-map))

(define-key chargebee-minor-mode-map (kbd "C-c g") 'chargebee--reload-customer)
(defun chargebee--reload-customer ()
  (interactive)
  (chargebee-customer chargebee-customer-id))

;; TODO - Add keys C-c n and C-c p to call previous and next button behaviors, to do that it's needed to encapsulate these behaviors into functions and control the limits between the pages

(defun chargebee-minor-mode-activate ()
  (setq chargebee-minor-mode t))

(provide 'chargebee-minor-mode)
