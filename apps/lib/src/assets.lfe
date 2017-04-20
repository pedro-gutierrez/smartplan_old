(defmodule assets
  (export all))

(defun create (u f next)
  (let* ((asset_id (maps:get #"id" f))
        (uid (maps:get #"id" u))
        (f2 (maps:merge f (map #"createdby" uid #"user" uid)))
        (size (maps:get #"size" f)))
    (db:hmset (list #"asset" asset_id) f2 (lambda (a)
      (app:incr #"assets" 1 (lambda (_) 
        (app:incr #"assetsSize" size (lambda (_)
          (funcall next a)))))))))

(defun with_asset (id next)
  (db:hgetall (list #"asset" id) next))

(defun with_asset (id)
  (with_asset id (lambda (m) m )))

(defun with_asset (id default next)
  (case (with_asset id)
    ((tuple 'error (tuple 'not_found _)) (with_asset default next))
    ((= _ m) (funcall next m))))

(defun get (_ id next)
  (with_asset id next))

(defun link (p asset next)
  (db:add_child p asset #"assets" next))

(defun with_assets (p next)
  (db:join
    (list #"assets" (maps:get #"id" p))
    #"asset"
    (list #"id" #"created" #"name" #"type" #"size" )
    #"created"
    next))
