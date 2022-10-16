(defmodule db (behaviour supervisor)
  (export all))

(defun start_link ()
  (supervisor:start_link #(local db) 'db '()))

(defun init (_)
  (tuple 'ok 
    (tuple 
      (tuple 'one_for_one 10 10)
      (list (poolboy:child_spec 'eredis '( 
        #(name #(local eredis)) 
        #(worker_module eredis) 
        #(size 10) 
        #(max_overflow 10)) '("redis", 6379, 0, ""))))))

(defun log (cmd)
  (case (kit:cfg 'redis 'verbose)
    ('true (io:format "[Redis] ~p~n" (list cmd)))
    ('false 'ok)))

(defun cmd (c next)
  (log c)
  (case (poolboy:transaction 'eredis (lambda (w) (eredis:q w c 5000)))
    ((tuple 'error e) (kit:err 'redis e))
    ((tuple 'ok r) 
      (log r)
      (funcall next r))))

(defun key 
  (((= (cons _ _) k)) (kit:joinbin k #"-"))
  ((k) k))

(defun field (e p)
  (kit:catbin (list e #"-*->" p)))

(defun fields (e props)
  (: lists map (lambda (p)
    (field e p)) props ))

(defun to_map (l)
  (to_map l #M()))

(defun to_map 
  (((cons k (cons v t)) m) (to_map t (maps:put k v m)))
  (('() m) m))

(defun to_list (m)
  (maps:fold (lambda (k v acc) 
    (cons k (cons v acc))) '() m))

(defun flushall ()
  (cmd (list #"FLUSHALL") (lambda (r) r)))

(defun hmset (k m next)
  (cmd (cons #"HMSET" (cons (key k) (to_list m)))
    (lambda (_) (hgetall k next ))))

(defun hmset (k m)
  (hmset k m (lambda (r) r)))

(defun hgetall (k next) 
  (cmd (list #"HGETALL" (key k)) (lambda(r)
    (case r 
      ('() (kit:err 'not_found k))
      ( _ (funcall next (to_map r)))))))

(defun hgetall (k) 
  (hgetall k (lambda (r) r)))

(defun hincrby (k field amount next)
  (cmd (list #"HINCRBY" (key k) field amount ) next))

(defun hincrby (k field amount) 
  (hincrby k field amount (lambda (r) r)))

(defun setnx (k next)
  (let ((id (kit:uuid)))
    (cmd (list #"SETNX" (key k) id ) (lambda (r)
      (case r 
        (#"1" (funcall next id))
        (_ (kit:err 'conflict k)))))))

(defun expire (k t next)
  (cmd (list #"EXPIRE" (key k) t ) (lambda (_)
    (funcall next))))

(defun del (k next)
  (cmd (list #"DEL" (key k)) (lambda (_)
    (funcall next))))

(defun del (k)
  (del k (lambda ())))

(defun get (k next) 
  (cmd (list #"GET" (key k)) (lambda (v)
    (case v 
      ('undefined (kit:err 'not_found k))
      (_ (funcall next v))))))

(defun get (k) 
  (get k (lambda (v) v)))

(defun sadd (k v next)
    (cmd (cons #"SADD" (cons (key k) v)) (lambda(_)
      (smembers k next))))
  
(defun sadd (k v)
  (sadd k v (lambda (v) v)))

(defun srem (k v next)
  (cmd (cons #"SREM" (cons (key k) v)) (lambda (_)
    (smembers k next))))

(defun srem (k v )
  (srem k v (lambda (s) s )))


(defun smembers (k next)
  (cmd (list #"SMEMBERS" (key k)) (lambda (members) 
    (funcall next (lists:map (lambda (m) (encode_raw_value m)) members )))))

(defun smembers (k)
  (smembers k (lambda (s) s)))

(defun sort_alpha (k next)
  (cmd (list #"SORT" (key k) #"ALPHA" ) (lambda (s)
    (funcall next s))))

(defun sort_alpha (k)
  (sort_alpha k (lambda(s) s)))

(defun prefix (p all)
  (lists:foldr (lambda (e acc)
    (cons p (cons e acc ))) () all))

(defun encode_raw_value 
  (('undefined) 'null)
  ((v) v))

(defun rs2maps 
  ((() _) '())
  ((s props) (rs2maps s props props #M() ())))

(defun rs2maps 
  ((() _ _ cmap out) (lists:reverse (cons cmap out)))
  ((s props () cmap out)
    (rs2maps s props props #M() (cons cmap out)))
  (((cons v t1) props (cons k t2) cmap out )
    (rs2maps t1 props t2 (maps:put k (encode_raw_value v) cmap) out )))

(defun join_alpha (k with props sortby next )
  (cmd (lists:flatten (list #"SORT" (key k) #"BY" (field with sortby) #"ALPHA" (prefix #"GET" (fields with props)) #"DESC"))
    (lambda(s) (funcall next (rs2maps s props)))))

(defun join (k with props next)
  (cmd (lists:flatten (list #"SORT" (key k) #"BY" #"NOSORT" (prefix #"GET" (fields with props))))
    (lambda(s) (funcall next (rs2maps s props)))))
  
(defun join (k with props sortby next )
  (join k with props sortby #"DESC" next))

(defun join (k with props sortby order next )
  (cmd (lists:flatten (list #"SORT" (key k) #"BY" (field with sortby) (prefix #"GET" (fields with props)) order ))
    (lambda(s) (funcall next (rs2maps s props)))))

(defun add_child (p c r next) 
  (sadd (list r (maps:get #"id" p)) (list (maps:get #"id" c)) 
    (lambda (_) (funcall next))))

(defun add_child (p c r)
  (add_child p c r (lambda () )))

(defun remove_child (p c r next)
  (srem (list r (maps:get #"id" p)) (list (maps:get #"id" c))
    (lambda (_) (funcall next))))

(defun remove_children (p r next)
  (del (list r (maps:get #"id" p)) next))

(defun remove_children! (p r t next)
  (remove_children! p r t (smembers (list r (maps:get #"id" p))) next))

(defun remove_children!
  ((p r t '() next)(remove_children p r next))
  ((p r t (cons id rem) next)
    (del (list t id) (lambda ()
      (remove_children! p r t rem next)))))

(defun hget (k f next)
  (cmd (list #"HGET" (key k ) f) (lambda (r)
    (case r 
      (#"nil" (kit:err 'not_found (lists:flatten k f)))
      (_ (funcall next r)))))) 

(defun hset (k f v next)
  (cmd (list #"HSET" (key k) f v ) next))

(defun hdel (k f next )
  (cmd (list #"HDEL" (key k) f ) next)) 

(defun sismember (k v)
  (cmd (list #"SISMEMBER" (key k) v) (lambda (r)
    (case r 
      (#"0" 'false)
      (#"1" 'true)))))

(defun lrem (k v next)
  (cmd (list #"LREM" (key k) 1 v) (lambda (_)
    (funcall next))))

(defun lpush (k v next)
  (cmd (list #"LPUSH" (key k) v ) (lambda (_)
    (funcall next))))

(defun rpop (k next)
  (cmd (list #"RPOP" (key k)) next))
 
(defun scard (k next)
  (cmd (list #"SCARD" k ) (lambda (r)
    (funcall next (kit:bin2int r)))))
