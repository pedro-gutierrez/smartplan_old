(defmodule notifications 
  (export all))

;; Returns all the notifications for the given
;; user
(defun get_all (u next)
  (map_notes (join_notes u #"notifications") next))

;; Returns only the unread notifications for 
;; the given user
(defun get_unread (u next)
  (map_notes (join_notes u #"notifications-unread") next))

;; Returns only the unread notifications for 
;; the given user
(defun get_read (u next)
  (map_notes (join_notes u #"notifications-read") next))

(defun join_notes (u k)
  (db:join
    (list k (maps:get #"id" u))
    #"notification"
    (list #"id" #"status" #"type" #"organization" #"shift" #"created" )
    #"created" 
    (lambda (notes) notes)))

(defun map_notes (notes next)
  (funcall next (lists:map (lambda (n)
    (map_note n (maps:get #"organization" n) (maps:get #"shift" n))) notes)))

(defun map_note
  ((n 'null sid)
    (schedules:with_schedule_shift_or_empty sid (lambda (s) 
      (maps:remove #"organization" (maps:put #"shift" s n)))))
  ((n oid 'null) 
    (organizations:with_organization_or_empty oid (lambda (o)
      (maps:remove #"shift" (maps:put #"organization" o n))))))

;; Marks all notifications as unread
;; for the given user
(defun read_all (u next)
  (get_unread u (lambda (notes)
    (lists:map (lambda (n) (read_one u (maps:get #"id" n) (lambda () ))) notes)
    (stats:set u #"notifications_unread" 0 (lambda (_)
      (funcall next))))))

(defun purge_one (u n next)
  (let ((id (maps:get #"id" n))
        (s (maps:get #"status" n)))
    (db:remove_child u n #"notifications" (lambda ()
      (db:remove_child u n (kit:joinbin (list #"notifications" s) #"-" ) (lambda ()
        (db:del (list #"notification" id) next)))))))

;; Deletes all notifications
(defun purge_read (u next)
  (let ((notes (join_notes u #"notifications-read")))
    (lists:map (lambda (n) (purge_one u n (lambda () ))) notes)
    (funcall next)))

;; Marks the specified notification as read
(defun read_one (u id next)
  (db:hmset (list #"notification" id) (map #"status" #"read") (lambda (n)
    (db:remove_child u n #"notifications-unread" (lambda ()
      (db:add_child u n #"notifications-read" (lambda ()
        (stats:decr u #"notifications_unread" (lambda (_)
          (funcall next))))))))))

;; Marks the specified notification as unread
(defun unread_one (u id next)
  (db:hmset (list #"notification" id) (map #"status" #"unread") (lambda (n)
    (db:add_child u n #"notifications-unread" (lambda ()
      (db:remove_child u n #"notifications-read" (lambda ()
        (stats:incr u #"notifications_unread" (lambda (_)
          (funcall next))))))))))

;; Adds a new unread notification for the given
;; user, type and organization
(defun organization (u t o next)
 (let ((uid (maps:get #"id" u))
       (id (kit:uuid)))
  (db:hmset (list #"notification" id) 
    (map #"id" id #"to" uid #"created" (kit:now) #"status" #"unread" #"organization" (maps:get #"id" o) #"type" t) 
    (lambda (n)
      (db:add_child u n #"notifications-unread" (lambda () 
        (db:add_child u n #"notifications" (lambda () 
          (stats:incr u #"notifications_unread" (lambda (_)
            (funcall next)))))))))))

;; Adds a new unread notification for the given
;; user, type, organization and shift
;; 
(defun shift (u t s next)
 (let ((uid (maps:get #"id" u))
       (id (kit:uuid)))
  (db:hmset (list #"notification" id) 
    (map #"id" id #"to" uid #"created" (kit:now) #"status" #"unread" #"shift" (maps:get #"id" s) #"type" t) 
    (lambda (n)
      (db:add_child u n #"notifications-unread" (lambda () 
        (db:add_child u n #"notifications" (lambda () 
          (stats:incr u #"notifications_unread" (lambda (_)
            (funcall next)))))))))))

