(defmodule engine_planner
  (behavior gen_fsm)
  (behavior gen_event)
  (export all))

(defun start_link () 
  (gen_fsm:start_link #(local engine_planner) 'engine_planner '() '()))

(defun init (args)
  (tuple 'ok 'starting #M() 0))

(defun on (e)
  (gen_fsm:send_event 'engine_planner e))

(defun starting
  (('timeout data) 
    (tuple 'next_state 'started data)))

(defun create_job (sid next)
  (on (tuple 'create sid))
  (funcall next))

;;
;; Convenience function that starts a timer
;; for the given job id and the given number of milliseconds
;;
(defun start_timer (e id ms data)
  (start_timer 'local e id ms data))

(defun start_timer (n e id ms data)
  (maps:put n 
    (gen_fsm:send_event_after ms (tuple e id)) data))

;;
;; Convenience function that cancels a timer
;; for the given job
;;
(defun cancel_timer (id data)
  (cancel_timer 'local id data))

(defun cancel_timer (n id data)
  (case (maps:is_key n data)
    ('true (gen_fsm:cancel_timer (maps:get n data)) data)
    ('false data )))

(defun cancel_timers (id data)
  (cancel_timer 'local id data)
  (cancel_timer 'global id data))

(defun log (e id)
  (engine_logger:add e id))
  ;(kit:log 'engine "[~p] engine job: ~p~n" (list e id))) 

(defun log (e id info)
  (engine_logger:add e id info))
  ;(kit:log 'engine "[~p] engine job: ~p, info: ~p~n" (list e id info))) 

(defun next_job (data)
  (case (engine_job:dequeue)
    ('undefined (tuple 'next_state 'started data))
    ((= _ id)
      (let ((data2 (start_timer 'run_timeout id (kit:cfg 'engine 'job_run_timeout) data)))
        (log 'running id)
        (engine_job:run id)
        (tuple 'next_state 'started data2)))))

(defun started

  ;;
  ;; A new job is about to be created. Do some checks, such as verifying the status
  ;; of the schedule and checking if there is already a job registered for that sid
  ;;
  (((tuple 'create sid) data)
    (schedules:with_schedule sid (lambda (s)
      (let ((status (maps:get #"status" s))
            (exists (engine_job:exists? sid)))
        (log 'create sid (tuple 'status (maps:get #"status" s) 'exists exists))
        (engine_job:unqueue sid)
        (case exists
          ('false 
            (engine_job:create sid))
          ('true 
            (engine_job:delete sid)
            (engine_job:create sid)))
        (tuple 'next_state 'started data)))))

  ;; 
  ;; A job indicated not enough data (shifts or employees)
  ;; is available
  (((tuple 'not_enough_data sid) data)
    (let* ((data2 (cancel_timers sid data))
           (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'not_enough_data sid)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))

  ;;
  ;; a new job is waiting - decide whether or not 
  ;; this or some other job with higher priority should be
  ;; started, according the max concurrency allowed in the 
  ;; system
  ;;
  ;;
  (((tuple 'waiting id) data)
    (log 'waiting id)
    (let ((r (engine_job:running))
          (c (kit:cfg 'engine 'concurrency)))
      (case (> c r)
        ('false (tuple 'next_state 'started data))
        ('true (next_job data)))))
  
  ;;
  ;; There was an attempt to run the specified 
  ;; job while it was already running 
  ;;
  (((tuple 'already_running sid) data)
      (let ((data2 (cancel_timers sid data)))
        (log 'already_running sid)
        (tuple 'next_state 'started data2)))

  ;;
  ;; the job that is currently running did finish 
  ;; the initialization and is now ready to start 
  ;;
  (((tuple 'ready sid) state)
    (log 'ready sid)
    (engine_job:start sid)
    (tuple 'next_state 'started state))
    
  ;;
  ;; An employee is participating in the 
  ;; matching process
  ;;
  (((tuple 'employee_matching sid uid) data)
    (log 'employee_matching sid (tuple 'uid uid))
    (tuple 'next_state 'started data))

  ;;
  ;; a running job finished the matching phase, and is now 
  ;; ready to start the swap phase
  ;; 
  (((tuple 'matched sid info) data)
    (let* ((data2 (cancel_timer sid data))
          (data3 (start_timer 'swap_timeout sid (kit:cfg 'engine 'job_swap_timeout) data2))) 
      (log 'matched sid info)
      (engine_job:swap sid)
      (tuple 'next_state 'started data3)))
 

  ;;
  ;; a running job finished the assign phase
  ;; 
  (((tuple 'assigned sid info) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'assigned sid info)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))


  ;;
  ;; a running job started a swap phase
  ;; for the moment we just log it
  ;; 
  (((tuple 'swapping sid) data)
    (log 'swapping sid)
    (tuple 'next_state 'started data))

  ;;
  ;;  
  ;;
  (((tuple 'ack_grant_swap sid uid) data)
    (log 'ack_grant_swap sid (tuple 'uid uid))
    (tuple 'next_state 'started data))
  
  ;;
  ;;  
  ;;
  (((tuple 'ack_deny_swap sid uid) data)
      (log 'ack_deny_swap sid (tuple 'uid uid))
      (tuple 'next_state 'started data))

  ;;
  ;;  
  ;;
  (((tuple 'swap_application sid uid) data)
      (log 'swap_application sid (tuple 'uid uid))
      (tuple 'next_state 'started data))
  ;;
  ;;  
  ;;
  (((tuple 'swap_pass sid uid) data)
      (log 'swap_pass sid (tuple 'uid uid))
      (tuple 'next_state 'started data))


  ;;
  ;;  
  ;;
  (((tuple 'swap_reply_nomatch sid uid) data)
      (log 'swap_reply_nomatch sid (tuple 'uid uid))
      (tuple 'next_state 'started data))

  ;;  
  ;;
  (((tuple 'swap_reply_full sid uid) data)
      (log 'swap_reply_full sid (tuple 'uid uid))
      (tuple 'next_state 'started data))


  ;;  
  ;;
  (((tuple 'swap_reply_partial sid uid) data)
      (log 'swap_reply_partial sid (tuple 'uid uid))
      (tuple 'next_state 'started data))

  
  ;;  
  ;;
  (((tuple 'swap_retain sid info) data)
      (log 'swap_retain sid info)
      (tuple 'next_state 'started data))


  ;;  
  ;;
  (((tuple 'swap_discard sid info) data)
      (log 'swap_discard sid info)
      (tuple 'next_state 'started data))
 
  ;;
  ;; A swap initiator did not configure any swap handshakes
  ;; We re-initiate a new swap phase
  ;;
  (((tuple 'no_swap_handshakes sid) data)
    (log 'no_swap_handshakes sid)
    (tuple 'next_state 'started data))

  ;;  
  ;;
  (((tuple 'swap_commit sid) data)
      (log 'swap_commit sid)
      (tuple 'next_state 'started data))

  ;;  
  ;;
  (((tuple 'swap_check sid) data)
      (log 'swap_check sid)
      (tuple 'next_state 'started data))

  ;;  
  ;;
  (((tuple 'swap_check_ok sid) data)
      (log 'swap_check_ok sid)
      (tuple 'next_state 'started data))
  
  ;;  
  ;;
  (((tuple 'swap_check_nok sid) data)
      (log 'swap_check_nok sid)
      (tuple 'next_state 'started data))

  ;;
  ;;
  (((tuple 'shift_ack_swap sid (tuple shid h s)) data)
    (log 'shift_ack_swap sid (tuple 'shift shid 'dh h 'ds s))
    (tuple 'next_state 'started data))

  ;;
  ;;
  (((tuple 'employee_ack_swap sid (tuple uid h s)) data)
    (log 'employee_ack_swap sid (tuple 'uid uid 'dh h 'ds s))
    (tuple 'next_state 'started data))


  ;;
  ;;   
  ;;
  (((tuple 'swapped sid info) data)
    (log 'swapped sid info)
    (tuple 'next_state 'started data))

  ;;
  ;;   
  ;;
  (((tuple 'finishing_swap sid) data)
    (log 'finishing_swap sid)
    (tuple 'next_state 'started data))


  ;;
  ;; A swap phase finished. We start a new one  
  ;;
  (((tuple 'swap_finished sid) data)
    (log 'swap_finished sid)
    (engine_job:swap sid)
    (tuple 'next_state 'started data))


  ;;
  ;; The current swap phase did not find a swap initiator
  ;; We stop the job
  ;;
  (((tuple 'no_swap_initiator sid) data)
    (let* ((data2 (cancel_timers sid data))
           (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'no_swap_initiator sid)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))

  ;;
  ;; a job finished calculating and reached a perfect solution
  ;; - we wait a grace period to let pending messages to 
  ;; - through
  ;;
  (((tuple 'finished sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'finished sid)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))
 
  ;;
  ;; Now we stop the job
  ;;
  (((tuple 'stop sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'stop sid )
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))
      
  ;;
  ;; a job did stop - enter the debrief stage 
  ;;
  (((tuple 'stopped sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'debrief_timeout sid (kit:cfg 'engine 'job_debrief_timeout) data2)))
      (log 'stopped sid)
      (engine_job:debrief sid)
      (tuple 'next_state 'started data3)))

  ;; 
  ;; a job did finish - delete it and proceed with the next job
  ;;
  (((tuple 'debriefed sid) data)
    (let ((data2 (cancel_timers sid data)))
      (log 'debriefed sid)
      (engine_job:delete sid)
      (next_job data))) 
  ;;
  ;; a job failed - delete it and proceed with the next job
  ;;
  (((tuple 'error sid) data)
    (let ((data2 (cancel_timers sid data)))
      (log 'error sid)
      (engine_job:delete sid)
      (next_job data)))

  ;;
  ;; a job timed out while running - stop it 
  ;;
  (((tuple 'run_timeout sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'run_timeout sid)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))
 
  ;;
  ;; a job timed out while swapping - 
  ;; we give it another chance
  ;;
  (((tuple 'swap_timeout sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'stop_timeout sid (kit:cfg 'engine 'job_stop_timeout) data2))) 
      (log 'swap_timeout sid)
      (engine_job:stop sid)
      (tuple 'next_state 'started data3)))

  ;;
  ;; a job timed out while stopping - debrief it 
  ;;
  (((tuple 'stop_timeout sid) data)
    (let* ((data2 (cancel_timers sid data))
          (data3 (start_timer 'debrief_timeout sid (kit:cfg 'engine 'job_debrief_timeout) data2))) 
      (log 'stop_timeout sid)
      (engine_job:debrief sid)
      (tuple 'next_state 'started data3)))
  
  ;;
  ;; a job timed out while debriefing - delete it
  ;;
  (((tuple 'debrief_timeout sid) data)
    (let ((data2 (cancel_timers sid data)))
      (log 'debrief_timeout sid)
      (case (engine_job:exists? sid)
        ('true 
          (engine_job:error sid)
          (tuple 'next_state 'started data2))
        ('false 
          (engine_job:mark sid #"error")
          (next_job data)))))
  )
