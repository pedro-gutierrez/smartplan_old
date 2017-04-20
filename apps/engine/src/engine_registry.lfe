(defmodule engine_registry
  (behavior gen_server)
  (export all))


(defun register (t id)
  (gen_server:cast 'engine_registry (tuple t id (self)))) 

(defun lookup (t id) 
  (gen_server:call 'engine_registry (tuple t id))) 

(defun with (t id next)
  (funcall next (lookup t id)))

(defun exists? (t id)
  (gen_server:call 'engine_registry (tuple 'check t id))) 

(defun start_link () 
  (gen_server:start_link #(local engine_registry) 'engine_registry '() '())) 

(defun init (args)
  (tuple 'ok 
    (map  'job            (ets:new 'jobs '())
          'job_sup        (ets:new 'job_sup ())
          'shift          (ets:new 'shifts '())
          'employee       (ets:new 'employees '())
          'shifts_sup     (ets:new 'shifts_sup '())
          'employees_sup  (ets:new 'employees_sup '())
          'weeks_sup      (ets:new 'weeks_sup '())
          'week           (ets:new 'week '()))))

(defun terminate (reason, state) 'ok)

(defun set (tables t k v)
  (ets:insert (maps:get t tables) (tuple k v) ))

(defun get (tables t k )
  (ets:lookup (maps:get t tables) k))

(defun handle_cast 
  (((tuple t id pid) tables) 
    (set tables t id pid) 
    (tuple 'noreply tables)))

(defun handle_call
  (((tuple t id) _ tables)
    (let (((list (tuple id pid)) (get tables t id))) 
      (tuple 'reply pid tables)))
  (((tuple 'check t id) _ tables)
    (case (get tables t id)
      ('() (tuple 'reply 'false tables))
      ((list (tuple _ _)) (tuple 'reply 'true tables))
      ((= _ r) 
        (io:format "Unexpected result from get tables ~p ~p: ~p~n" (list t id r))
        (tuple 'reply 'error tables)))))
  
