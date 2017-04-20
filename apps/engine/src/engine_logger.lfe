(defmodule engine_logger
  (behavior gen_fsm)
  (export all))

(defun start_link () 
  (gen_fsm:start_link #(local engine_logger) 'engine_logger '() '()))

(defun init (args)
  (tuple 'ok 'starting #M() 0))

(defun add (e sid)
  (gen_fsm:send_event 'engine_logger (tuple e sid)))

(defun add (e sid info)
  (gen_fsm:send_event 'engine_logger (tuple e sid info)))

(defun starting
  (('timeout data) 
    (tuple 'next_state 'started data)))

(defun log (e id)
  (kit:log 'engine "[~p] [~p]~n" (list id e))) 

(defun log (e id info)
  (kit:log 'engine "[~p] [~p] [~p]~n" (list id e info))) 


(defun clear_events (id next)
  (schedules:with_schedule id (lambda (s)
    (common:clear_events s next))))


(defun log_event (id t data)
  (log_event id #"info" t data))

(defun log_event (id sev t data)
  (schedules:with_schedule id (lambda (s)
    (common:add_event s (map #"severity" sev #"type" t) (lambda (e)
      (log t id)
      (tuple 'next_state 'started data))))))

(defun log_event_with_info (id t info data)
  (log_event_with_info id #"info" t info data))

(defun log_event_with_info (id sev t info data)
  (schedules:with_schedule id (lambda (s)
    (common:add_event s (map #"severity" sev #"type" t #"info" (kit:map2string info #":" #",")) (lambda (e)
      (log t id info)
      (tuple 'next_state 'started data))))))


(defun started

  (((tuple 'create sid (tuple 'status s 'exists e)) data)
    (clear_events sid (lambda ()
      (log_event sid #"created" data))))
  
  (((tuple 'waiting sid) data)
    (log_event sid #"waiting" data)) 

  (((tuple 'already_running sid) data)
    (log_event sid #"warn" #"already_running" data)) 
  
  (((tuple 'running sid) data)
    (log_event sid #"running" data)) 

  (((tuple 'ready sid) data)
    (log_event sid #"ready" data)) 
  
  (((tuple 'employee_matching sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"employee_matching" (map #"uid" uid) data)) 

  (((tuple 'assigned sid (tuple 'staffing_total staffing_t 'staffing st 'hard_total hard_t 'hard h 'soft_total soft_t 'soft s)) data)
    (log_event_with_info sid #"assigned" 
      (map #"staffing_total" (kit:int2bin staffing_t) #"staffing" (kit:int2bin st) 
           #"hard_total" (kit:int2bin hard_t) #"hard" (kit:int2bin h) 
           #"soft_total" (kit:int2bin soft_t) #"soft" (kit:int2bin s)) data)) 
  
  (((tuple 'matched sid (tuple 'staffing_total staffing_t 'staffing st 'hard_total hard_t 'hard h 'soft_total soft_t 'soft s)) data)
    (log_event_with_info sid #"matched" 
      (map #"staffing_total" (kit:int2bin staffing_t) #"staffing" (kit:int2bin st) 
           #"hard_total" (kit:int2bin hard_t) #"hard" (kit:int2bin h) 
           #"soft_total" (kit:int2bin soft_t) #"soft" (kit:int2bin s)) data)) 

  (((tuple 'swapping sid) data)
    (log_event sid #"swapping" data)) 

  (((tuple 'swap_application sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"ask_swap" (map #"uid" uid)  data))

  (((tuple 'swap_pass sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"pass_swap" (map #"uid" uid)  data))

  (((tuple 'ack_grant_swap sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"grant_swap" (map #"uid" uid) data))

  (((tuple 'ack_deny_swap sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"deny_swap" (map #"uid" uid) data))
      
  (((tuple 'swap_reply_nomatch sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"reply_nomatch" (map #"uid" uid) data))
  
  (((tuple 'swap_reply_full sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"reply_full" (map #"uid" uid) data))
  
  (((tuple 'swap_reply_partial sid (tuple 'uid uid)) data)
    (log_event_with_info sid #"reply_partial" (map #"uid" uid) data))

  (((tuple 'swap_retain sid (tuple (tuple oshift ouid pshift uid) (tuple odh ods odv) (tuple pdh pds pdv))) data)
    (log_event_with_info sid #"retain_swap" 
      (map #"uid" uid #"odh" (kit:int2bin odh) #"ods" (kit:int2bin ods) #"odv" (kit:int2bin odv) 
                      #"pdh" (kit:int2bin pdh) #"pds" (kit:int2bin pds) #"pdv" (kit:int2bin pdv)) data))

  (((tuple 'swap_retain sid (tuple (tuple ouid pshift uid) (tuple odh ods odv) (tuple pdh pds pdv))) data)
    (log_event_with_info sid #"retain_swap" 
      (map #"uid" uid #"odh" (kit:int2bin odh) #"ods" (kit:int2bin ods) #"odv" (kit:int2bin odv)
                      #"pdh" (kit:int2bin pdh) #"pds" (kit:int2bin pds) #"pdv" (kit:int2bin pdv)) data))
  
  (((tuple 'swap_discard sid (tuple (tuple oshift ouid pshift uid) (tuple odh ods odv) (tuple pdh pds pdv))) data)
    (log_event_with_info sid #"discard_swap" 
      (map #"uid" uid #"odh" (kit:int2bin odh) #"ods" (kit:int2bin ods) #"odv" (kit:int2bin odv)
                      #"pdh" (kit:int2bin pdh) #"pds" (kit:int2bin pds) #"pdv" (kit:int2bin pdv)) data))

  (((tuple 'swap_discard sid (tuple (tuple ouid pshift uid) (tuple odh ods odv) (tuple pdh pds pdv))) data)
    (log_event_with_info sid #"discard_swap" 
      (map #"uid" uid #"odh" (kit:int2bin odh) #"ods" (kit:int2bin ods) #"odv" (kit:int2bin odv)   
                      #"pdh" (kit:int2bin pdh) #"pds" (kit:int2bin pds) #"pdv" (kit:int2bin pdv)) data))

  (((tuple 'no_swap_handshakes sid) data)
    (log_event sid #"warn" #"no_swap_handshake" data))

  (((tuple 'swap_commit sid) data)
    (log_event sid #"commit_swap" data))

  (((tuple 'swap_check sid) data)
    (log_event sid #"check_swap" data))
  
  (((tuple 'swap_check_ok sid) data)
    (log_event sid #"check_swap_ok" data))
  
  (((tuple 'swap_check_nok sid) data)
    (log_event sid #"check_swap_nok" data))
  
  (((tuple 'shift_ack_swap sid (tuple 'shift shid 'dh h 'ds s)) data)
    (log_event_with_info sid #"shift_ack_swap" (map #"shid" shid #"dh" (kit:int2bin h) #"ds" (kit:int2bin s)) data)) 

  (((tuple 'employee_ack_swap sid (tuple 'uid uid 'dh h 'ds s)) data)
    (log_event_with_info sid #"employee_ack_swap" (map #"uid" uid #"dh" (kit:int2bin h) #"ds" (kit:int2bin s)) data))

  (((tuple 'swapped sid (tuple 'staffing_total staffing_t 'staffing st 'hard_total hard_t 'hard h 'soft_total soft_t 'soft s)) data)
    (log_event_with_info sid #"swapped"
      (map #"staffing_total" (kit:int2bin staffing_t) #"staffing" (kit:int2bin st)
           #"hard_total" (kit:int2bin hard_t) #"hard" (kit:int2bin h)
           #"soft_total" (kit:int2bin soft_t) #"soft" (kit:int2bin s)) data))

  (((tuple 'finishing_swap sid) data)
    (log_event sid #"finishing_swap" data))

  (((tuple 'swap_finished sid ) data)
    (log_event sid #"swap_finished" data))

  (((tuple 'no_swap_initiator sid) data)
    (log_event sid #"warn" #"no_swap_initiator" data))
  
  (((tuple 'finished sid) data)
    (log_event sid #"finished" data))

  (((tuple 'not_enough_data sid) data)
    (log_event sid #"not_enough_data" data))
  
  (((tuple 'stop sid) data)
    (log_event sid #"stop" data))

  (((tuple 'stopped sid) data)
    (log_event sid #"stopped" data))
  
  (((tuple 'debriefed sid) data)
    (log_event sid #"debrief" data))

  (((tuple 'error sid) data)
    (log_event sid #"error" #"error" data))

  (((tuple 'run_timeout sid) data)
    (log_event sid #"warn" #"run_timeout" data))

  (((tuple 'swap_timeout sid) data)
    (log_event sid #"swap_timeout" data))

  (((tuple 'stop_timeout sid) data)
    (log_event sid #"warn" #"stop_timeout" data))

  (((tuple 'debrief_timeout sid) data)
    (log_event sid #"warn" #"debrief_timeout" data))


  )
