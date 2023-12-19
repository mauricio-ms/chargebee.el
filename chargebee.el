;;; -*- lexical-binding: t -*-
(defgroup chargebee nil
  "An interactive Chargebee environment for emacs."
  :tag "CHARGEBEE")

(defcustom chargebee-api-server nil
  "Chargebee API server environment."
  :tag "CHARGEBEE"
  :type 'string
  :group 'chargebee)

(defcustom chargebee-api-key nil
  "Chargebee API Key."
  :tag "CHARGEBEE"
  :type 'string
  :group 'chargebee)

(require 'load-relative)
(require 'widget)
(require-relative "./chargebee-minor-mode.el")

(defgroup chargebee-faces nil
  "Chargebee Faces."
  :group 'chargebee)

(defface chargebee-section-head
  '((((background  dark)) :foreground "green")
    (((background light)) :foreground "black"))
  "Face for section head."
  :group 'chargebee-faces)

(defface chargebee-yellow
  '((((background  dark)) :foreground "yellow")
    (((background light)) :foreground "black"))
  "Yellow face."
  :group 'chargebee-faces)

(defface chargebee-red
  '((((background  dark)) :foreground "red")
    (((background light)) :foreground "black"))
  "Red face."
  :group 'chargebee-faces)

(defface chargebee-highlight
  '((((class color) (background dark)) :extend t
     :weight ultra-bold))
  "Basic face for highlighting."
  :group 'chargebee-faces)

(defun chargebee-customer (chargebee-customer-id)
  (interactive "sCustomer Id: ") 
  (let ((buffer (get-buffer-create "*Chargebee-Customer*")))
    (with-current-buffer buffer
      (setq-local chargebee-customer-id chargebee-customer-id)
      (erase-buffer))

    (deferred:$
      (deferred:$
	(request-deferred (format "https://%s.chargebee.com/api/v2/customers/%s" chargebee-api-server chargebee-customer-id)
			  :type "GET"
			  :headers `(("Content-Type" . "application/json")
				     ,(chargebee--authorization-header))
			  :parser 'json-read)
	(deferred:nextc it
	   (lambda (response)
	     (if (not (= 200 (request-response-status-code response)))
	 	 (deferred:fail (format "Customer %s not found" customer-id))
	       response)))
	(deferred:nextc it
	  (lambda (response)
	    (let ((customer (get-attr (request-response-data response) '(customer))))
	      (with-current-buffer buffer
		(insert (format "Customer:   %s\n" (get-attr customer '(id))))
		(insert (format "First Name: %s\n" (get-attr customer '(first_name))))
		(insert (format "Last Name:  %s\n" (get-attr customer '(last_name))))
		(insert (format "Email:      %s\n" (get-attr customer '(email))))
		(insert ?\n)
		(insert ?\n)
		(chargebee--add-section-head "Billing Address")
		(insert (format "First Name: \t%s\n" (get-attr customer '(billing_address first_name))))
		(insert (format "Last Name:  \t%s\n" (get-attr customer '(billing_address last_name))))
		(insert (format "Company:    \t%s\n" (get-attr customer '(billing_address company))))
		(insert (format "Email:      \t%s\n" (get-attr customer '(billing_address email))))
		(insert (format "Phone:      \t%s\n" (get-attr customer '(billing_address phone))))
		(insert (format "Line 1:     \t%s\n" (get-attr customer '(billing_address line1))))
		(insert (format "Line 2:     \t%s\n" (get-attr customer '(billing_address line2))))
		(insert (format "Line 3:     \t%s\n" (get-attr customer '(billing_address line3))))
		(insert (format "City:       \t%s\n" (get-attr customer '(billing_address city))))
		(insert (format "Zip:        \t%s\n" (get-attr customer '(billing_address zip))))
		(insert (format "Country:    \t%s\n" (get-attr customer '(billing_address country))))
		(insert (format "State:      \t%s\n" (get-attr customer '(billing_address state))))
		(insert (format "Status:     \t%s\n" (get-attr customer '(billing_address validation_status))))
		(insert ?\n)
		(insert ?\n)))))
	(deferred:nextc it
	  (lambda (response)
	    (chargebee--write-invoices buffer chargebee-customer-id nil '()))))

      (deferred:error it
	(lambda (err)
	  (with-current-buffer buffer
	    (insert (nth 1 err)))))
      
      (deferred:nextc it
	(lambda (response)
	  (with-current-buffer buffer
	    (outline-minor-mode)
	    (setq-local outline-regexp ">")

	    ;; Set keybinding for toggling sections
	    (define-key outline-minor-mode-map (kbd "<tab>") 'outline-toggle-children)
	    (font-lock-mode)

	    (chargebee-minor-mode-activate)

	    (switch-to-buffer buffer)
	    (add-hook 'post-command-hook #'chargebee--highlight-current-line nil t)
	    (goto-line 1)
	    (chargebee--highlight-current-line)))))))

(defun chargebee--write-invoices (buffer customer-id offset pages-history)
  (message "chargebee--write-invoices: %s %s" offset pages-history)
  (deferred:$
    (request-deferred (format "https://%s.chargebee.com/api/v2/invoices"
			      chargebee-api-server)
		      :type "GET"
		      :params `(
				("customer_id[is]" . ,customer-id)
				("limit" . "10")
				("offset" . ,(or offset "")))
		      :headers `(("Content-Type" . "application/json")
				 ,(chargebee--authorization-header))
		      :parser 'json-read)
  
  (deferred:nextc it
    (lambda (response)
      (let* ((invoices-response (request-response-data response))
	     (invoices (get-attr invoices-response '(list))))
	(with-current-buffer buffer
	  (chargebee--add-section-head (format "Invoices (%d)" (seq-length invoices)))
	  (seq-do #'chargebee--write-invoice invoices)
	  (if pages-history
	      (progn
		(widget-create 'link
			       :notify (lambda (&rest ignore)
					 (save-excursion
					   (goto-char (point-min))
					   (forward-line 22)
					   (delete-region (point) (point-max)))
					 (let ((previous-offset (pop pages-history)))
					   (chargebee--write-invoices buffer customer-id previous-offset pages-history)))
			       "PREVIOUS")
		(insert "  ")))
	  (let ((next-offset (get-attr invoices-response '(next_offset))))
	    (if next-offset
		(progn
		  (widget-create 'link
				 :notify (lambda (&rest ignore)
					   (save-excursion
					     (goto-char (point-min))
					     (forward-line 22)
					     (delete-region (point) (point-max)))
					   (push offset pages-history)
					   (chargebee--write-invoices buffer customer-id next-offset pages-history))
				 "NEXT"))))
	  (insert ?\n)))))))

(defun chargebee--write-invoice (invoice)
  (let ((currency-code (get-attr invoice '(invoice currency_code))))
    (insert (propertize (format "%-10s" (get-attr invoice '(invoice id))) 
			'font-lock-face 'chargebee-yellow))

    (insert (format "%-20s %s %s"
		    ;; TODO - 3600 hardcoded to solveconsider daylight saving, check how it's the appropriate way to handle it
		    (format-time-string "%Y-%m-%dT%T" (get-attr invoice '(invoice date)) 3600)
		    currency-code 
		    (chargebee--to-decimal (get-attr invoice '(invoice total)))))

    (let ((refund-amount (chargebee--get-refund-amount-for-invoice invoice)))
      (if refund-amount
	  (insert (propertize (format " credits issued %s %s" currency-code refund-amount) 'font-lock-face 'chargebee-red))))
    (insert ?\n)))

(defun chargebee--highlight-current-line ()
  "Apply bold face to the current line in the buffer."
  (let ((current-line (line-number-at-pos)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line-start-pos (line-beginning-position))
	      (line-end-pos (line-end-position)))
	  (if (= (line-number-at-pos) current-line)
              (overlay-put (make-overlay line-start-pos line-end-pos) 'face 'chargebee-highlight)
	    (progn
	      ;; GAMBIARRA to not remove widget button feature
	      (let ((line-start (buffer-substring line-start-pos (1+ line-start-pos)))
		    (line-end (buffer-substring (1- line-end-pos) line-end-pos)))
		(if (not (and (string= "[" line-start) (string= "]" line-end)))
	            (chargebee--delete-overlays-specifying 'face)))))
          (forward-line))
        ))))

(defun chargebee--delete-overlays-specifying (prop)
  (let ((overlays (overlays-at (+ 1 (point)))))
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun chargebee--authorization-header ()
  `("Authorization" . ,(concat "Basic " (base64-encode-string (concat chargebee-api-key ":")))))

(defun chargebee--add-section-head (text)
  (insert (propertize (format "> %s\n" text) 'font-lock-face 'chargebee-section-head)))

(defun chargebee--get-refund-amount-for-invoice (invoice)
  (let ((issued-credit-notes (get-attr invoice '(invoice issued_credit_notes)))
	(amount 0))
    (seq-do
     (lambda (credit-note)
       (if (string= "refunded" (get-attr credit-note '(cn_status)))
	   (setq amount (+ amount (get-attr credit-note '(cn_total))))))
     issued-credit-notes)
    (if (> amount 0)
    	(chargebee--to-decimal amount))))

(defun chargebee--to-decimal (x)
  (/ x 100.0))

(defun get-attr (list keys)
  "Access the nested attributes specified by KEYS in LIST."
  (if keys
      (let ((current (car keys)))
        (if (listp list)
            (get-attr (cdr (assoc current list)) (cdr keys))
          (signal 'wrong-type-argument (list 'listp list))))
    list))

(provide 'chargebee)
