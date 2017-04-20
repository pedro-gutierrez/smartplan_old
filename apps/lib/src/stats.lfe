(defmodule stats
  (export all))

(defun get (_ id)
  (case (db:hgetall (list #"stats" id ))
    ((tuple 'error (tuple 'not_found _ )) #M())
    ((= _ v) v)))

;; Returns the stats for the specified
;; user or resource
(defun get (r)
  (case (db:hgetall (list #"stats" (maps:get #"id" r )))
    ((tuple 'error (tuple 'not_found _ )) #M())
    ((= _ v) v)))

(defun incr (u f next)
  (incr u f 1 next))

(defun incr (u f v next)
  (db:hincrby (list #"stats" (maps:get #"id" u)) f v next))

;; Decrements a stat by a value of 1, on the user's
;; global statistic object
(defun decr (u f next)
  (decr u f -1 next))

;; Decrements a stat on the user's global statistics 
;; object
(defun decr (u f v next)
  (db:hincrby (list #"stats" (maps:get #"id" u)) f v next))

;; Sets a statistic on the user's global
;; statistics object
(defun set (u f v next)
  (db:hset (list #"stats" (maps:get #"id" u)) f v next))

;; Builds the key to the weekly user stats
(defun with_weekly_stats_key (u t y w next)
  (funcall next (list #"stats" t #"w" (maps:get #"id" u) (kit:int2bin y) (kit:int2bin w))))

;; Builds a sort key for the given time period
(defun with_weekly_stats_sort_key (y w next)
  (funcall next (kit:bin2int (kit:joinbin
    (list (kit:int2bin y) (iolist_to_binary (io_lib:format "~2..0B" (list w))))))))


(defun stats_props ()
  (list #"id" #"total_training_time_w" #"total_std_time_w" #"max_std_time_w" #"total_idle_time_w" #"total_extra_time_w" #"year" #"week" #"sort" #"ownedby" ))
  


;; Returns all the weekly stats for the given user
(defun with_weekly_stats (u t next)
  (db:join
    (list #"stats" t #"w" (maps:get #"id" u))
    #"stats"
    (stats_props)
     #"stats"
     next))


(defun with_stats (p r next) 
  (db:join
    (list r (maps:get #"id" p))
     #"stats"
     (stats_props)
     #"sort"
     next))
  
(defun with_stats (p r props next) 
  (db:join
    (list r (maps:get #"id" p))
     #"stats"
     props
     #"sort"
     next))


(defun map_bins 
  ((_ '() _ _ out) out)
  ((mapFn (cons k rem) default in out)
    (map_bins mapFn rem default in
      (maps:put k 
        (funcall mapFn 
          (case (maps:is_key k in)
            ('false default)
            ('true (maps:get k in)))) out))))

  

(defun map_bins (mapfn keys default m)
  (map_bins mapfn keys default m m))
  
;; Converts binary values into floats
(defun parse_weekly_stats (stats)
  (map_bins 
    (lambda (n) (kit:bin2num n)) 
    (list #"total_std_time_w" #"max_std_time_w" #"total_training_time_w" #"total_idle_time_w" #"total_extra_time_w")
    #"0.0" 
    stats))

;; Converts float values as binaries
(defun format_weekly_stats (stats next)
  (funcall next (kit:mfloat2bin stats)))

(defun with_new_weekly_stats (u y w next)
  (with_weekly_stats_sort_key y w (lambda (sk)
    (funcall next (map 
      #"ownedby" (maps:get #"id" u) 
      #"year" y #"week" w #"sort" sk 
      #"total_std_time_w" 0.0 
      #"max_std_time_w" 0.0
      #"total_training_time_w" 0.0
      #"total_idle_time_w" 0.0 
      #"total_extra_time_w" 0.0)))))

;; Retrieves the statistics for the given
;; user and the given week
(defun with_weekly_stats (u t y w next)
  (with_new_weekly_stats u y w (lambda (default)
    (with_weekly_stats_key u t y w (lambda (k)
      (case (db:get k)
        ((tuple 'error _)(funcall next default))
        ((= _ id)
          (db:hgetall (list #"stats" id ) (lambda (stats)
            (let ((merged (maps:merge default (parse_weekly_stats stats))))
              (funcall next merged)))))))))))

(defun update (stats next)
  (format_weekly_stats stats (lambda (stats2)
    (db:hmset (list #"stats" (maps:get #"id" stats2)) stats2 next))))


;; Updates the stats for the given user
;; and week
(defun update_weekly_stats (u t y w stats next)
  (with_weekly_stats_key u t y w (lambda (k)
    (case (db:get k)
      ((tuple 'error _) 
        (db:setnx k (lambda (id)
          (let ((stats2 (maps:put #"id" id stats)))
            (update stats2 (lambda (stats3)
              (db:add_child u stats3 (kit:joinbin (list #"stats" t #"w") #"-") (lambda()
                (funcall next stats3)))))))))
      ((= _ id)
        (update (maps:put #"id" id stats) next))))))
