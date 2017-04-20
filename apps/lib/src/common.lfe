(defmodule common 
  (export all))

(defun with_access 
  ((u '() next) (funcall next '()))
  ((u (= (cons h t) all) next) 
    (funcall next (lists:map (lambda (i) (with_access u i (lambda (i2) i2))) all)))
  ((u t next)
    (let ((user_id (maps:get #"id" u)))
      (case (== user_id (maps:get #"ownedby" t ))
        ('true (funcall next (maps:put 'access 'owner t)))
        ('false
          (let ((organizations (db:smembers (list #"organizations" user_id)))
                (s (maps:get #"organization" t)))
            (case (lists:member s organizations)
              ('false (funcall next (maps:put 'access 'none t)))
              ('true (funcall next (maps:put 'access 'read t))))))))))

(defun with_user_access (u p next)
 (let ((uid (maps:get #"id" u))
       (pid (maps:get #"id" p)))
  (case (== uid pid )
    ('true (funcall next (maps:put 'access 'owner p)))
    ('false
      (let ((u_orgs (sets:from_list (db:smembers (list #"organizations" uid))))
            (p_orgs (sets:from_list (db:smembers (list #"organiations" pid)))))
        (case (sets:is_disjoint u_orgs p_orgs)
          ('false (funcall next (maps:put 'access 'none p)))
          ('true (funcall next (maps:put 'access 'read p)))))))))


(defun with_org_access (u o next)
  (let ((uid (maps:get #"id" u)))
    (case (== uid (maps:get #"ownedby" o))
      ('true (funcall next (maps:put 'access 'owner o)))
      ('false
        (let ((orgs (db:smembers (list #"organizations" uid)))
              (oid (maps:get #"id" o)))
          (case (lists:member oid orgs)
            ('false (funcall next (maps:put 'access 'none o)))
            ('true (funcall next (maps:put 'access 'read o)))))))))

(defun if_read (t next)
  (case (maps:get 'access t ) 
    ('owner (funcall next))
    ('read (funcall next))
    (_ (kit:err 'forbidden 'access))))

(defun if_owner (t next)
  (case (maps:get 'access t ) 
    ('owner (funcall next))
    (_ (kit:err 'forbidden 'access))))

(defun is_owner (t)
  (== 'owner (maps:get 'access t)))

(defun if_not_owner (t next) 
  (case (maps:get 'access t)
    ('owner (kit:err 'forbidden 'cant_be_owner))
    (_ (funcall next))))

(defun is_read (t)
  (== 'read (maps:get 'access t)))

(defun is_access (t)
  (or (is_owner t) (is_read t)))

(defun filter_with_access (items next)
  (funcall next (lists:filter #'is_access/1 items)))

(defun add_contrib (u t next)
  (let ((id (kit:uuid)))
    (db:hset (list #"contribs" (maps:get #"id" u)) (maps:get #"id" t) id (lambda (_)
      (db:hmset (list #"contrib" id) (map #"id" id) next)))))

(defun remove_contrib (u t next)
  (with_contrib u t (lambda (c)
    (db:del (list #"contrib" (maps:get #"id" c)) (lambda ()
      (db:hdel (list #"contribs" (maps:get #"id" u)) (maps:get #"id" t) (lambda (_) 
        (funcall next))))))))

(defun with_contrib (u t next)
  (db:hget (list #"contribs" (maps:get #"id" u)) (maps:get #"id" t) (lambda (id)
    (db:hgetall (list #"contrib" id) next))))

(defun add_tag (u t tag next)
  (with_contrib u t (lambda (c)
    (db:sadd (list #"tags" (maps:get #"id" c)) (list (kit:bin2lower tag)) next ))))

(defun add_tag (t tag next)
  (db:sadd (list #"tags" (maps:get #"id" t)) (list (kit:bin2lower tag)) next ))

(defun add_tags 
  ((t '() next) (get_tags t next))
  ((t tags next)
    (db:sadd (list #"tags" (maps:get #"id" t)) 
      (lists:map (lambda(tag) (kit:bin2lower tag)) tags) next)))

(defun remove_tag (u t tag next )
  (with_contrib u t (lambda (c)
    (db:srem (list #"tags" (maps:get #"id" c)) (list (kit:bin2lower tag)) next ))))

(defun remove_tag (t tag next)
  (db:srem (list #"tags" (maps:get #"id" t)) (list (kit:bin2lower tag)) next ))

(defun get_tags (u t next)
  (with_contrib u t (lambda (c)
    (db:smembers (list #"tags" (maps:get #"id" c)) next ))))

(defun get_tags (t next)
  (db:smembers (list #"tags" (maps:get #"id" t )) next ))

(defun with_tags (t next) 
  (db:smembers (list #"tags" (maps:get #"id" t )) next ))

(defun with_tag (t tag next)
  (get_tags t (lambda (tags) 
    (case (lists:member tag tags)
      ('true (funcall next tag))
      ('false (kit:err 'not_found tag))))))

(defun on_new_tag (t tag next) 
  (get_tags t (lambda (tags)
    (case (lists:member (kit:bin2lower tag) tags)
      ('true (kit:err 'conflict tag))
      ('false (funcall next))))))

(defun match_tags (s1 s2)
  (sets:is_subset (sets:from_list s1) (sets:from_list s2)))

(defun on_new_name (u ns name next)
  (case (db:sismember (list #"names" (maps:get #"id" u) ns) (kit:bin2lower name))
    ('false (funcall next))
    ('true (kit:err 'conflict name ))))

(defun add_name (u ns name next)
  (db:sadd (list #"names" (maps:get #"id" u) ns) (list (kit:bin2lower name)) (lambda(_) 
    (funcall next))))

(defun remove_name (u ns name next)
  (db:srem (list #"names" (maps:get #"id" u) ns) (list (kit:bin2lower name)) (lambda(_) 
    (funcall next))))

(defun set_status (ns kind t new next)
  (let ((old (maps:get #"status" t))
        (id (maps:get #"id" t)))
    (db:hmset (list kind id) (map #"status" new) (lambda (t2)
      (db:srem (list ns old) (list id) (lambda (_)
        (db:sadd (list ns new) (list id) (lambda (_)
          (funcall next t2)))))))))

(defun count_status (ns status next )
  (db:scard (list ns status) next))

(defun count_status (ns status )
  (count_status ns status (lambda (r) r)))

(defun queue (q id next) 
  (db:lpush (list #"q" q ) id next ))

(defun queue (q id)
  (queue q id (lambda () 'ok )))

(defun dequeue (q next)
  (db:rpop (list #"q" q ) next))

(defun dequeue (q)
  (dequeue q (lambda (r) r )))

(defun unqueue (q val next)
  (db:lrem (list #"q" q) val next))

(defun unqueue (q val)
  (unqueue q val (lambda () )))

;;
;; Returns the stats for the given
;; object
;; 
(defun with_stats (o next)
  (case (db:hgetall (list #"stats" (maps:get #"id" o))) 
    ((tuple 'error (tuple 'not_found _)) (funcall next #M())) 
    ((= _ stats) (funcall next stats))))

(defun if_status 
  ((r () next) (kit:err 'forbidden 'wrong_status))
  ((r (cons s rest) next)
    (case (== s (maps:get #"status" r))
      ('true (funcall next))
      ('false (if_status r rest next)))))
 

(defun clear_events (p next)
  (with_events p (list #"id") (lambda (events)
    (lists:map (lambda (e) (db:del (list #"event" (maps:get #"id" e)))) events)
    (db:remove_children p #"events" next))))
  
(defun add_event (p props next)
  (let* ((id (kit:uuid))
         (e (maps:merge props (map #"id" id #"created" (kit:now_micro)))))
    (db:hmset (list #"event" id) e (lambda (e2)
      (db:add_child p e2 #"events" (lambda ()
        (kit:log 'events "[event] ~p~n" (list e))
        (funcall next e2)))))))

(defun with_events (p next)
  (with_events p (list #"created" #"type" #"severity" #"info") next))

(defun with_events (p props next)
  (db:join
    (list #"events" (maps:get #"id" p))
    #"event"
    props
    #"created"
    #"ASC"
    next))


(defun add_comment (u p text next)
  (let ((id (kit:uuid)))
    (db:hmset (list #"comment" id)
      (map #"id" id #"text" text #"createdby" (maps:get #"id" u) #"created" (kit:now)) (lambda (c)
        (db:add_child p c #"comments" (lambda ()
          (funcall next c)))))))

(defun with_comments (p next)
  (db:join
    (list #"comments" (maps:get #"id" p))
    #"comment"
    (list #"id" #"created" #"createdby" #"text")
    #"created"
    #"ASC"
    next))
  
