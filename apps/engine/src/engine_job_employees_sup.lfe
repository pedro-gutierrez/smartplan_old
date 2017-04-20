(defmodule engine_job_employees_sup 
  (behaviour supervisor)
  (export all))

(defun create (sid o u)
  (engine_registry:with 'employees_sup sid (lambda (pid)
    (supervisor:start_child pid (list o u)))))

(defun start_link (id) 
  (supervisor:start_link 'engine_job_employees_sup (list id)))

(defun init (((list sid))
  (engine_registry:register 'employees_sup sid)
  (tuple 'ok (tuple (tuple 'simple_one_for_one 0 1 ) (list 
    (tuple 'engine_job_employee (tuple 'engine_job_employee 'start_link (list sid)) 'temporary 'brutal_kill 'worker (list 'engine_job_employee)))))))
