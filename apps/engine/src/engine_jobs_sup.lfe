(defmodule engine_jobs_sup 
  (behaviour supervisor)
  (export all))

(defun start_link () 
  (supervisor:start_link #(local engine_jobs_sup) 'engine_jobs_sup '()))

(defun init (args) 
  #(ok #(#(simple_one_for_one 0 1) (
    #(engine_job_sup #(engine_job_sup start_link ()) temporary 500 supervisor (engine_job_sup))))))

(defun create (id)
  (supervisor:start_child 'engine_jobs_sup (list id)))

(defun delete (sid)
  (supervisor:terminate_child 'engine_jobs_sup (engine_job_sup:lookup sid) )) 
