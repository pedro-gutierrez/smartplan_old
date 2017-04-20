(defmodule engine_job_shifts_sup 
  (behaviour supervisor)
  (export all))

(defun create (sid shift)
  (engine_registry:with 'shifts_sup sid (lambda (pid)
    (supervisor:start_child pid (list shift)))))

(defun start_link (sid) 
  (supervisor:start_link 'engine_job_shifts_sup (list sid)))

(defun init (((list sid))
  (engine_registry:register 'shifts_sup sid)
  (tuple 'ok (tuple (tuple 'simple_one_for_one 0 1 ) (list 
    (tuple 'engine_job_shift (tuple 'engine_job_shift 'start_link (list sid)) 'temporary 'brutal_kill 'worker (list 'engine_job_shift)))))))
