(defmodule engine_sup 
  (behaviour supervisor)
  (export all))

(defun start_link () 
  (supervisor:start_link #(local engine_sup) 'engine_sup '()))

(defun init (args) 
  #(ok #(#(one_for_one 0 1) (
    #(engine_planner  #(engine_planner  start_link ()) permanent 2000 worker      (engine_planner))
    #(engine_logger   #(engine_logger   start_link ()) permanent 2000 worker      (engine_logger))
    #(engine_notifier #(engine_notifier start_link ()) permanent 2000 worker      (engine_notifier))
    #(engine_registry #(engine_registry start_link ()) permanent 2000 worker      (engine_registry))
    #(engine_jobs_sup #(engine_jobs_sup start_link ()) permanent 2000 supervisor  (engine_jobs_sup))))))
