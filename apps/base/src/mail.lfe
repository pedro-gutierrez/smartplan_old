(defmodule mail
  (behavior gen_server)
  (export all))

(defun send (t s b)
  (gen_server:cast 'mail (tuple 'mail t s b))
  (tuple 'notification (kit:cfg 'mail 'enabled)))

(defun send (f t s b) 
  (send (map #"From" f #"To" t #"Subject" s #"HtmlBody" (kit:joinbin b))))

(defun send (msg) 
  (case (kit:cfg 'mail 'enabled)
    ('false #(notification false))
    ('true
      (let ((payload (jiffy:encode msg))) 
        (case (httpc:request 'post (tuple 
          (kit:cfg 'mail 'provider)
          (list 
            (tuple "X-Postmark-Server-Token" (kit:cfg 'mail 'provider_key))
            (tuple "Accept" "application/json")
            (tuple "Content-Type" "application/json")
            (tuple "Content-Length" (byte_size payload)))
          "application/json"
          payload) '() '())
            ((tuple 'ok _) #(notification true))
            (_ (kit:err 'mail 'unknown )))))))

(defun start_link () 
  (gen_server:start_link #(local mail) 'mail '() '()))

(defun init (args)
  (tuple 'ok #M()))

(defun terminate (reason, state) 'ok)

(defun handle_cast 
  (((tuple 'mail (= (cons h t) tos) subject body) state)
    (lists:map (lambda (to) (send (kit:cfg 'mail 'from) to subject body)) tos)
    (tuple 'noreply state))
  (((tuple 'mail to subject body) state)
    (send (kit:cfg 'mail 'from) to subject body)
    (tuple 'noreply state)))
