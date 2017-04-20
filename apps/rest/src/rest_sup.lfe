(defmodule rest_sup 
  (behaviour supervisor)
  (export all))

(defun start_link () 
  (supervisor:start_link #(local rest_sup) 'rest_sup '()))

(defun init (args) 
  #(ok #(#(one_for_one 0 1) (
    #(db  #(db  start_link ()) permanent 2000 supervisor (db))
    #(mail  #(mail  start_link ()) permanent 2000 supervisor (db))))))
