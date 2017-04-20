(defmodule engine 
  (behaviour application)
  (export all))

(defun start (type, args)
  (engine_sup:start_link))

(defun start() 
  (application:start 'engine))

(defun stop (state) 'ok )

(defun stop () 
  (application:stop 'engine))

(defun create_job (id next) 
  (engine_planner:create_job id next))
