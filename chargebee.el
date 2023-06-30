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

(require 'widget)

(defgroup chargebee-faces nil
  "Faces used by Chargebee."
  :group 'chargebee)

(defface chargebee-section-head
  '((((background  dark)) :foreground "green")
    (((background light)) :foreground "black"))
  "Fringe face for current position."
  :group 'chargebee-faces)

;; TODO - Check if it will be used or not
(defface chargebee-button-temp4
  '((((background  dark)) :foreground "orange" :weight ultra-bold)
    (((background light)) :foreground "black" :weight ultra-bold))
  "Fringe face for current position."
  :group 'chargebee-faces)

(defface chargebee-yellow
  '((((background  dark)) :foreground "yellow")
    (((background light)) :foreground "black"))
  "Fringe face for current position."
  :group 'chargebee-faces)

(defface chargebee-red
  '((((background  dark)) :foreground "red")
    (((background light)) :foreground "black"))
  "Fringe face for current position."
  :group 'chargebee-faces)

(defface chargebee-highlight
  '((((class color) (background dark)) :extend t
     :weight ultra-bold))
  "Basic face for highlighting."
  :group 'chargebee-faces)

(defun chargebee-customer ()
  (interactive)
  (let ((buffer (get-buffer-create "*Chargebee-Customer*")))
    (with-current-buffer buffer
      (erase-buffer))

    (deferred:$
      (request-deferred (format "https://%s.chargebee.com/api/v2/customers/%s" chargebee-api-server customer-id)
			:type "GET"
			:headers `(("Content-Type" . "application/json")
				   ,(chargebee--authorization-header))
			:parser 'json-read)
      (deferred:nextc it
	(lambda (response)
	  (let ((customer (get--attr (request-response-data response) '(customer))))
	    (with-current-buffer buffer
	      (insert (format "Customer:   %s\n" (get--attr customer '(id))))
	      (insert (format "First Name: %s\n" (get--attr customer '(first_name))))
	      (insert (format "Last Name:  %s\n" (get--attr customer '(last_name))))
	      (insert (format "Email:      %s\n" (get--attr customer '(email))))
	      (insert ?\n)
	      (insert ?\n)
	      (chargebee--add-section-head "Billing Address")
	      (insert (format "First Name: \t%s\n" (get--attr customer '(billing_address first_name))))
	      (insert (format "Last Name:  \t%s\n" (get--attr customer '(billing_address last_name))))
	      (insert (format "Company:    \t%s\n" (get--attr customer '(billing_address company))))
	      (insert (format "Email:      \t%s\n" (get--attr customer '(billing_address email))))
	      (insert (format "Phone:      \t%s\n" (get--attr customer '(billing_address phone))))
	      (insert (format "Line 1:     \t%s\n" (get--attr customer '(billing_address line1))))
	      (insert (format "Line 2:     \t%s\n" (get--attr customer '(billing_address line2))))
	      (insert (format "Line 3:     \t%s\n" (get--attr customer '(billing_address line3))))
	      (insert (format "City:       \t%s\n" (get--attr customer '(billing_address city))))
	      (insert (format "Zip:        \t%s\n" (get--attr customer '(billing_address zip))))
	      (insert (format "Country:    \t%s\n" (get--attr customer '(billing_address country))))
	      (insert (format "State:      \t%s\n" (get--attr customer '(billing_address state))))
	      (insert (format "Status:     \t%s\n" (get--attr customer '(billing_address validation_status))))
	      (insert ?\n)
	      (insert ?\n)
	      )))))

    
    (chargebee--write-invoices buffer nil)
    (with-current-buffer buffer
      (outline-minor-mode)
      (setq-local outline-regexp ">")
      ;; Hide all sections
      ;; (outline-hide-body)
      ;; Set keybinding for toggling sections
      (define-key outline-minor-mode-map (kbd "<tab>") 'outline-toggle-children)
      (font-lock-mode)

      ;; TODO - Implement pagination (use limit to specify the items per page, use next_offset parameter of the response to pass in the offset parameter of the next request)
      (switch-to-buffer buffer)
      ;; (add-hook 'post-command-hook #'chargebee--highlight-current-line nil t) TODO - FIX IT TO NO REMOVE WIDGET FUNCTIONALITY
      ;; TODO - Buttons next and previous, should use go-to function to rewrite the contents, previous pages needs to be cached
      )))
;; (chargebee-customer)

;; TODO - Continue function to use goto and replace the invoices instead generate new sections
;; TODO - Calculate the local to write the invoices content, because the request can finish before that the header part request
(defun chargebee--write-invoices (buffer offset)
  (message "chargebee--write-invoices: %s" offset)
  (deferred:$
    (request-deferred (format "https://%s.chargebee.com/api/v2/invoices"
			      chargebee-api-server)
		      :type "GET"
		      :params `(
				("customer_id[is]" . ,customer-id)
				("limit" . "2")
				("offset" . ,(or offset "")))
		      :headers `(("Content-Type" . "application/json")
				 ,(chargebee--authorization-header))
		      :parser 'json-read)
  
  (deferred:nextc it
    (lambda (response)
      (let* ((invoices-response (request-response-data response))
	     (invoices (get--attr invoices-response '(list))))
	(with-current-buffer buffer
	  (chargebee--add-section-head (format "Invoices (%d)" (seq-length invoices)))
	  (seq-do #'chargebee--write-invoice invoices)
	  (let ((next-offset (get--attr invoices-response '(next_offset))))
	    (message "NEXT: %s" next-offset)
	    (if next-offset
		(widget-create 'link
			       :notify (lambda (&rest ignore)
					 (chargebee--write-invoices buffer next-offset))
			       "NEXT") 
	      ;; (insert (propertize "NEXT" 'font-lock-face 'chargebee-button-temp4))
	      ))
	  (insert ?\n)))))))

(defun chargebee--write-invoice (invoice)
  (let ((currency-code (get--attr invoice '(invoice currency_code))))
    (insert (propertize (format "%-10s" (get--attr invoice '(invoice id))) 
			'font-lock-face 'chargebee-yellow))

    (insert (format "%-20s %s %s"
		    ;; TODO - 3600 hardcoded to solveconsider daylight saving, check how it's the appropriate way to handle it
		    (format-time-string "%Y-%m-%dT%T" (get--attr invoice '(invoice date)) 3600)
		    currency-code 
		    (chargebee--to-decimal (get--attr invoice '(invoice total)))))

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
        (if (= (line-number-at-pos) current-line)
            (overlay-put (make-overlay (line-beginning-position) (line-end-position)) 'face 'chargebee-highlight)
          (chargebee--delete-overlays-specifying 'face))
        (forward-line)))))

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
  (let ((issued-credit-notes (get--attr invoice '(invoice issued_credit_notes)))
	(amount 0))
    (seq-do
     (lambda (credit-note)
       (if (string= "refunded" (get--attr credit-note '(cn_status)))
	   (setq amount (+ amount (get--attr credit-note '(cn_total))))))
     issued-credit-notes)
    (if (> amount 0)
    	(chargebee--to-decimal amount))))

(defun chargebee--to-decimal (x)
  (/ x 100.0))

(provide 'chargebee)
