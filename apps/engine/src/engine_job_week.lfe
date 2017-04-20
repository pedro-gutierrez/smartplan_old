(defmodule engine_job_week
  (behavior gen_fsm)
  (export all))

(defun start_link (sid u y w) 
  (gen_fsm:start_link 'engine_job_week (list sid u y w) '()))

(defun register (sid u y w)
  (engine_registry:register 'week (kit:catbin (list sid (maps:get #"id" u) y w))))

(defun lookup (sid uid y w)
  (engine_registry:lookup 'week (kit:catbin (list sid uid y w))))

(defun send (sid uid y w e)
  (gen_fsm:send_event (lookup sid uid y w) e))

(defun debrief (sid uid y w)
  (send sid uid y w 'begin_debrief))

(defun finish_debrief(sid uid y w)
  (send sid uid y w 'finish_debrief))

(defun shift_starts (sid uid y w shift)
  (send sid uid y w (tuple 'shift_starts shift)))

(defun shift_ends (sid uid y w shift)
  (send sid uid y w (tuple 'shift_ends shift)))

(defun shift_duration (sid uid y w shift)
  (send sid uid y w (tuple 'shift_duration shift)))


;;
;; clean stop of an employee process
;;
(defun stop (sid uid y w)
  (gen_fsm:send_all_state_event (lookup sid uid y w) 'stop))

(defun init (((list sid u y w))
  (register sid u y w)
  (tuple 'ok 'starting (map 'sid sid 'u u 'y y 'w w ) 0)))

(defun update_weekly_stats (shift data next)
  (schedules:add_shift_duration_to_weekly_stats shift (maps:get 'stats data) (lambda (stats2)
    (funcall next (maps:put 'stats stats2 data)))))

(defun check_extra_time (shift data next)
  (let ((oid (maps:get #"organization" shift)))
    (with_profile_rules oid data (lambda (data2 rules)
      (schedules:apply_rule_to_shift 'extra_time_w rules shift (maps:get 'stats data2) (lambda (shift2 stats2)
        (funcall next shift2 (maps:put 'stats stats2 data2))))))))

(defun check_conflict (s1 s2 data next)
  (with_latest_ending_shift s1 s2 (lambda (latest)
    (let ((data2 (maps:put 'shift latest data)))
      (case (schedules:shift_overlap? s1 s2)
        ('false (funcall next data2))
        ('true 
          (schedules:set_shift_conflict s1(lambda (_)
            (schedules:set_shift_conflict s2 (lambda (_)
              (funcall next data2)))))))))))

(defun finish_debrief (data)
  (let* ((sid (maps:get 'sid data))
         (u (maps:get 'u data))
         (uid (maps:get #"id" u))
         (y (kit:bin2int (maps:get 'y data)))
         (w (kit:bin2int (maps:get 'w data)))
         (stats (maps:get 'stats data)))
    (stats:update_weekly_stats u #"scheduled" y w stats (lambda(_)
      (engine_job:debriefed sid)
        (tuple 'next_state 'debriefed data)))))

(defun with_latest_ending_shift (s1 s2 next)
  (let ((k1 (kit:bin2int (schedules:shift_end_key s1)))
        (k2 (kit:bin2int (schedules:shift_end_key s2))))
    (case (> k1 k2)
      ('true (funcall next s1))
      ('false (funcall next s2)))))

(defun with_profile_rules (oid data next)
  (let ((rules (maps:get 'rules data)))
    (case (maps:is_key oid rules) 
      ('true (funcall next data (maps:get oid rules)))
      ('false
        (let ((u (maps:get 'u data)))
          (organizations:with_organization oid (lambda (o)
            (schedules:with_organization_member_rules u o (lambda (r)
              (funcall next (maps:put 'rules (maps:put oid r rules) data) r))))))))))  

(defun starting
  
  ;;
  ;; Delayed initialization of the employee 
  ;; process. Get the tags for the employee in the 
  ;; context of the given organization and notify the job
  ;; that we are waiting to starting matching
  ;;
  (('timeout data) 
    (let* ((sid (maps:get 'sid data))
          (u (maps:get 'u data))
          (uid (maps:get #"id" u))
          (y (maps:get 'y data))
          (w (maps:get 'w data))
          (data2 (maps:put 'rules #M() data)))
      (tuple 'next_state 'waiting data2 ))))

(defun waiting

  ;;
  ;; We get a request from the job process 
  ;; to start the debriefing phase
  ;;
  (('begin_debrief data)
    (let* ((sid (maps:get 'sid data))
          (u (maps:get 'u data))
          (uid (maps:get #"id" u))
          (y (maps:get 'y data))
          (w (maps:get 'w data)))
      (stats:with_new_weekly_stats u (kit:bin2int y) (kit:bin2int w) (lambda (stats)
        (schedules:with_scheduled_weekly_shifts u y w (lambda (shifts)
          (lists:map (lambda (shift) (engine_job_week:shift_starts sid uid y w shift)) shifts)
          (engine_job_week:finish_debrief sid uid y w)
          (tuple 'next_state 'debriefing (maps:put 'stats stats data)))))))))


(defun debriefing
  
  ;; 
  ;; No more shifts - finish the debriefing 
  ;; process and notify the job
  (('finish_debrief data)
    (finish_debrief data))

  ;;
  ;; First shift starts - just update the weekly stats
  ;;
  (((tuple 'shift_starts shift) data)
    (update_weekly_stats shift data (lambda (data2)
      (check_extra_time shift data2 (lambda (shift2 data3)
        (tuple 'next_state 'shift (maps:put 'shift shift2 data3))))))))

(defun shift
  
  ;;
  ;; A new shift starts - update weekly stats
  ;; and check for overlaps
  ;; 
  (((tuple 'shift_starts shift) data)
    (let ((prev (maps:get 'shift data)))
      (update_weekly_stats shift data (lambda (data2)
        (check_extra_time shift data2 (lambda (shift2 data3)
          (check_conflict prev shift2 data3 (lambda (data4)
            (tuple 'next_state 'shift data4)))))))))

  ;; 
  ;; No more shifts - finish the debriefing 
  ;; process and notify the job
  (('finish_debrief data)
    (finish_debrief data)))

;; All state events, such as errors and timeouts and 
;; the stop command
;;
(defun handle_event
  
  ;;
  ;; for the moment we dont have anything here
  ;; if there is stale data in transit but not 
  ;; handled by a shift process then it will be lost
  ;;
  (('stop state data)
    (let* ((sid (maps:get 'sid data))
          (u (maps:get 'u data))
          (uid (maps:get 'uid data))
          (y (maps:get 'y data))
          (w (maps:get 'w data)))
      (engine_job:week_stopped sid uid y w)
      (tuple 'next_state 'end data)))) 
    
;;
;; Shutdown of the state machine. We need to distinguish
;; different cases, in particular, if this is a local failure,
;; inform the job process so that we clean up the schedule
;; state
;;
(defun terminate 
  (('normal _ _))
  ((_ _ data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:error sid))))
