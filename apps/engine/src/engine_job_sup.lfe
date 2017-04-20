(defmodule engine_job_sup 
  (behaviour supervisor)
  (export all))

(defun lookup (sid) 
  (engine_registry:lookup 'job_sup sid))

(defun register (sid) 
  (engine_registry:register 'job_sup sid))  

(defun start_link (id) 
  (supervisor:start_link 'engine_job_sup (list id)))

(defun init (((list id)) 
  (register id)
  (tuple 'ok (tuple (tuple 'one_for_one 0 1 ) (list 
    (tuple 'engine_job (tuple 'engine_job 'start_link (list id)) 'permanent 2000 'worker (list 'engine_job))
    (tuple 'engine_job_weeks_sup (tuple 'engine_job_weeks_sup 'start_link (list id)) 'permanent 2000 'supervisor (list 'engine_job_weeks_sup))
    (tuple 'engine_job_shifts_sup (tuple 'engine_job_shifts_sup 'start_link (list id)) 'permanent 2000 'supervisor (list 'engine_job_shifts_sup))
    (tuple 'engine_job_employees_sup (tuple 'engine_job_employees_sup 'start_link (list id)) 'permanent 2000 'supervisor (list 'engine_job_employees_sup)))))))
