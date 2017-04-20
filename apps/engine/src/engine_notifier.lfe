(defmodule engine_notifier
  (behavior gen_event)
  (export all))

(defun start_link () 
  (let (((tuple 'ok pid) (gen_event:start_link #(local engine_notifier))))
    (register 'engine_notifier)
    (tuple 'ok pid)))

(defun init (args ) 
  #(ok #M()))

(defun register (h) 
  (gen_event:add_handler 'engine_notifier h '()))

(defun emit (e id) 
  (gen_event:notify 'engine_notifier (tuple e id)))

(defun emit (e id info) 
  (gen_event:notify 'engine_notifier (tuple e id info)))

(defun handle_event (e state)
  (engine_planner:on e)
  (tuple 'ok state))
