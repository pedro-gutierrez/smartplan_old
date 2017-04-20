(defmodule issues 
  (export all))

(defun with_issues (u status next)
  (db:join
    (list #"issues" status)
    #"issue"
    (list #"id" #"num" #"created" #"createdby" #"ownedby" #"status" #"summary" #"estimate" #"release" )
    #"created" 
    next))

(defun with_issue (u id next)
  (db:hgetall (list #"issue" id) (lambda (i)
    (with_access u i next))))

(defun get (u status)
  (with_issues u status (lambda (issues) 
    (lists:map (lambda (i) (with_access u i (lambda (i2) i2))) issues))))

(defun i18n_status
  ((#"new" lang) (kit:i18n 'status_new lang))
  ((#"progress" lang)(kit:i18n 'status_progress lang))
  ((#"resolved" lang)(kit:i18n 'status_resolved lang))
  ((#"closed" lang)(kit:i18n 'status_closed lang)))

(defun send_note 
  ((to f lang i 'created next)
    (mail:send to (kit:i18n 'issue_created_subject lang (list (maps:get #"num" i)))
      (list
        (kit:i18n 'hello lang (list f))
        (kit:i18n 'issue_created_body lang (list (maps:get #"num" i) (maps:get #"summary" i)))
        (kit:i18n 'issue_footer lang)
        (kit:i18n 'footer lang)))
    (funcall next))
  ((to f lang i 'comment next)
    (mail:send to (kit:i18n 'issue_comment_subject lang (list (maps:get #"num" i)))
      (list
        (kit:i18n 'hello lang (list f))
        (kit:i18n 'issue_comment_body lang (list (maps:get #"text" i) (maps:get #"num" i) (maps:get #"summary" i)))
        (kit:i18n 'issue_footer lang)
        (kit:i18n 'footer lang)))
    (funcall next))
  ((to f lang i 'status next)
    (let ((status (i18n_status (maps:get #"status" i) lang)))
      (mail:send to (kit:i18n 'issue_status_subject lang (list (maps:get #"num" i) status))
        (list
          (kit:i18n 'hello lang (list f))
          (kit:i18n 'issue_status_body lang (list (maps:get #"num" i) status (maps:get #"summary" i)))
          (kit:i18n 'issue_footer lang)
          (kit:i18n 'footer lang)))
      (funcall next)))
  ((to f lang i 'release next)
    (let ((release (maps:get #"release" i)))
      (mail:send to (kit:i18n 'issue_release_subject lang (list (maps:get #"num" i) release))
        (list
          (kit:i18n 'hello lang (list f))
          (kit:i18n 'issue_release_body lang (list (maps:get #"num" i) release (maps:get #"summary" i)))
          (kit:i18n 'issue_footer lang)
          (kit:i18n 'footer lang)))
      (funcall next)))
  ((to f lang i 'asset next)
    (mail:send to (kit:i18n 'issue_asset_subject lang (list (maps:get #"num" i)))
      (list
        (kit:i18n 'hello lang (list f))
        (kit:i18n 'issue_asset_body lang (list (maps:get #"num" i) (maps:get #"summary" i)))
        (kit:i18n 'issue_footer lang)
        (kit:i18n 'footer lang)))
      (funcall next)))
    
    
(defun send_note (to i e next)
  (let ((lang (kit:lang (maps:get #"lang" to)))
        (email (maps:get #"email" to))
        (f (maps:get #"first" to)))
    (send_note email f lang i e next)))        

(defun send_notes
  (('() _ _ next) (funcall next))
  (((cons to rem) i e next)
    (send_note to i e (lambda ()
      (send_notes rem i e next)))))

(defun has_issue_emails (u)
  (case (maps:is_key #"issue_emails" u)
    ('false 'false)
    ('true (== #"true" (maps:get #"issue_emails" u) ))))

(defun notify (author i e next)
  (case (kit:cfg 'issues 'notifications)
    ('false (funcall next))
    ('true 
      (users:find_by_acl #"issues" (lambda (users)
        (send_notes (lists:filter (lambda (u) 
          (and (=/= (maps:get #"id" u) (maps:get #"id" author)) (has_issue_emails u))) users) i e next))))))

(defun create (u summary next)
  (app:incr #"bugs" 1 (lambda (n)
    (let ((owner (kit:cfg 'issues 'owner))
          (author (maps:get #"id" u))
          (id (kit:uuid))
          (s #"new"))
      (db:hmset (list #"issue" id)
        (map #"id" id #"num" n #"summary" summary #"createdby" author #"created" (kit:now) #"ownedby" owner #"status" s) (lambda (issue)
          (db:sadd (list #"issues" s) (list id) (lambda (_)
            (update_status_bag id 'none s (lambda ()
              (add_event u issue #"status" s (lambda (_)
                (notify u issue 'created (lambda ()
                  (with_access u issue next)))))))))))))))

(defun create (u summary)
  (create u summary (lambda (s) s )))

(defun rename (u i summary next)
  (db:hmset (list #"issue" (maps:get #"id" i)) (map #"summary" summary) next))

(defun rename (u i summary) 
  (rename u i summary (lambda (i2)
    (with_access u i2 (lambda (i3) i3)))))

(defun update_status (u i new_status next)
  (let ((id (maps:get #"id" i))
        (old_status (maps:get #"status" i)))
    (case (== old_status new_status)
      ('false
        (db:hmset (list #"issue" id) (map #"status" new_status) (lambda (i2)
          (db:srem (list #"issues" old_status) (list id) (lambda (_)
            (db:sadd (list #"issues" new_status) (list id) (lambda (_)
              (update_status_bag id old_status new_status (lambda ()
                (add_event u i2 #"status" new_status (lambda (_)
                  (notify u i2 'status (lambda ()
                    (with_access u i2 next))))))))))))))
      ('true (funcall next i)))))

(defun update_status (u i new_status)
  (update_status u i new_status (lambda (i2) i2)))
  
(defun update_status_bag
  ((id _ #"closed" next)
    (db:srem (list #"issues" #"open") (list id) (lambda (_)
      (db:sadd (list #"issues" #"closed") (list id) (lambda (_)
        (funcall next))))))
  ((id #"closed" _ next)
    (db:srem (list #"issues" #"closed") (list id) (lambda (_)
      (db:sadd (list #"issues" #"open") (list id) (lambda (_)
        (funcall next))))))
  ((id 'none _ next)
    (db:sadd (list #"issues" #"open") (list id) (lambda (_)
      (funcall next))))
  ((id _ _ next ) (funcall next)))

(defun add_asset (u i m next) 
  (assets:link i m (lambda ()
    (add_event u i #"asset" (maps:get #"name" m) (lambda (_) 
      (notify u i 'asset (lambda ()
        (funcall next))))))))

(defun add_event (u i t v next)
  (let ((id (kit:uuid))
        (owner (maps:get #"id" u)))
    (db:hmset (list #"event" id)
      (map #"id" id #"issue" (maps:get #"id" i) #"type" t #"value" v #"created" (kit:now) #"ownedby" owner )
      (lambda (e) (db:add_child i e #"events" (lambda () (funcall next e)))))))

(defun add_event (u i t next)
  (let ((id (kit:uuid))
        (owner (maps:get #"id" u)))
    (db:hmset (list #"event" id)
      (map #"id" id #"issue" (maps:get #"id" i) #"type" t #"created" (kit:now) #"ownedby" owner )
      (lambda (e) (db:add_child i e #"events" (lambda () (funcall next e)))))))


(defun with_events (u i next)
  (db:join
    (list #"events" (maps:get #"id" i))
    #"event"
    (list #"id" #"issue" #"created" #"ownedby" #"type" #"value"  )
    #"created" 
    next))

(defun get_events (u i)
  (with_events u i (lambda (e) e)))

(defun get_comments (u i)
  (common:with_comments i (lambda (c) c)))

(defun create_comment (u i text)
  (common:add_comment u i text (lambda (c2) 
    (add_event u i #"comment" text (lambda (_)
      (notify u (maps:merge i c2) 'comment (lambda () c2)))))))

(defun toggle_emails (u next)
  (case (has_issue_emails u)
    ('true (users:set u #"issue_emails" 'false next))
    ('false (users:set u #"issue_emails" 'true next))))

(defun update_estimate (u i estimate next)
  (db:hmset (list #"issue" (maps:get #"id" i)) (map #"estimate" estimate) (lambda (i2)
    (with_access u i2 next)))) 

(defun update_estimate (u i estimate)
  (update_estimate u i estimate (lambda (e) e)))

(defun update_release (u i release next)
  (db:hmset (list #"issue" (maps:get #"id" i)) (map #"release" release) (lambda (i2)
    (add_event u i2 #"release" release (lambda (_)
      (notify u i2 'release (lambda ()
        (with_access u i2 next)))))))) 

(defun update_release (u i release)
  (update_release u i release (lambda (e) e)))

(defun with_access (u i next) 
  (case (== (maps:get #"id" u) (maps:get #"ownedby" i))
    ('true (funcall next (maps:put 'access 'owner i )))
    ('false (funcall next (maps:put 'access 'contrib i)))))
