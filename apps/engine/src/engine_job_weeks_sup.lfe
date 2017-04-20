(defmodule engine_job_weeks_sup
  (behaviour supervisor)
  (export all))

(defun create (sid u y w)
  (engine_registry:with 'weeks_sup sid (lambda (pid)
    (supervisor:start_child pid (list u y w)))))

(defun start_link (id) 
  (supervisor:start_link 'engine_job_weeks_sup (list id)))

(defun init (((list sid))
  (engine_registry:register 'weeks_sup sid)
  (tuple 'ok (tuple (tuple 'simple_one_for_one 0 1 ) (list 
    (tuple 'engine_job_week (tuple 'engine_job_week 'start_link (list sid)) 'temporary 'brutal_kill 'worker (list 'engine_job_week)))))))
