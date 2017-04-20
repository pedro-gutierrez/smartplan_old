(defmodule engine_job_shift
  (behavior gen_fsm)
  (export all))

(defun start_link (sid shift)
  (gen_fsm:start_link 'engine_job_shift (list sid shift) '()))

(defun register (s) 
  (engine_registry:register 'shift (maps:get #"id" s)))

(defun lookup (shid)
  (engine_registry:lookup 'shift shid))

(defun send(sid e)
  (engine_registry:with 'shift sid (lambda (pid) 
    (gen_fsm:send_event pid e))))

(defun assign (shid)
  (send shid 'assign))


(defun match (shid emps)
  (send shid (tuple 'match emps)))


;;
;; This function is called by employee processes
;; that wish not to match
(defun pass_match (shid)
  (send shid 'pass_match))

;; 
;;
;; This function is called by employee processes
;; that wish to match on the shift
;;
(defun query_match (shid uid)
  (send shid (tuple 'query_match uid)))

;;
;; This function is called by employee processes
;; in order to acknowledge a succesfull match
;;
(defun ack_match (shid uid)
  (send shid (tuple 'ack_match uid)))

;;
;; This function is called by the employee 
;; initiator process that wants to start a new swap
(defun query_swap (shid uid shifts)
  (send shid (tuple 'query_swap uid shifts)))

;;
;; This function is called by the job when
;; the swap phase is about to commit. We need to check the swap first
;;
(defun check_swap (shid from to)
  (send shid (tuple 'check_swap from to)))

;; This function is called by the job when
;; the swap phase has been checked and it is ok to actually
;; apply it
;;
(defun swap (shid from to)
  (send shid (tuple 'swap from to)))

;;
;; This function is called by the job process 
;; on those shifts that are not involved in the 
;; swap process
;;
(defun finish_swap (shid)
  (send shid 'finish_swap))

;;
;; This function is called by employee processes
;; as they finish applying shift transfers
;;
(defun ack_swap (shid uid h s)  
  (send shid (tuple 'ack_swap uid h s)))

;;
;; Clean stop of a shift process
;;
(defun stop (shid)
  (gen_fsm:send_all_state_event (lookup shid) 'stop))

;;
;; Perform a swap between two employees
;;
(defun do_swap (from to data)
  (let* ((sid (maps:get 'sid data))
         (shift (maps:get 'shift data))
         (shid (maps:get 'shid data))
         (matched_shifts (maps:get 'matched_shifts data))
         (ushid (maps:get from matched_shifts))
         (matched_shifts2 (maps:put to ushid matched_shifts))
         (matched_shifts3 (maps:remove from matched_shifts2))
         (stdelta  (- (maps:size matched_shifts3) (maps:size matched_shifts)))
         (data2 (maps:put 'stdelta stdelta data)))
        (engine_job_employee:confirm_swap sid to 'add shift)
        (engine_job_employee:confirm_swap sid from 'remove shift)
        (tuple 'next_state 'swapping (maps:merge data2 (map 'matched_shifts matched_shifts3 'swap_confirm_queries 2)))))

(defun do_check_swap (from to data)
  (let* ((sid (maps:get 'sid data))
         (shift (maps:get 'shift data))
         (shid (maps:get 'shid data))
         (matched_shifts (maps:get 'matched_shifts data))
         (ushid (maps:get from matched_shifts))
         (matched_shifts2 (maps:put to ushid matched_shifts))
         (matched_shifts3 (maps:remove from matched_shifts2))
         (stdelta  (- (maps:size matched_shifts3) (maps:size matched_shifts)))
         (data2 (maps:put 'stdelta stdelta data)))
  (case (>= stdelta 0)
    ('false 
      (engine_job:swap_check_nok sid shid from to)
      (tuple 'next_state 'swapping data2))
    ('true 
      (engine_job:swap_check_ok sid shid from to)
      (tuple 'next_state 'swapping data2)))))

(defun init (((list sid shift))
  (let ((shid (maps:get #"id" shift)))
    (engine_registry:register 'shift shid)
    (tuple 'ok 'starting 
      (map 'sid sid 'shid shid 'shift shift ) 0 ))))

(defun starting
  ;;
  ;; Delayed initialization of the shift process
  ;; Here we unwind the template shift into individual, unassigned
  ;; shifts
  ;;
  (('timeout data)
    (let ((sid (maps:get 'sid data))
          (shid (maps:get 'shid data))
          (shift (maps:get 'shift data)))
      (schedules:with_schedule sid (lambda (s)
        (case (maps:get #"auto" s)
          (#"false"
            (engine_job:shift_waiting sid shid 1)
            (tuple 'next_state 'waiting (maps:put 's s data)))
          (#"true"
            (schedules:with_shift_tags shift (lambda (tags)
              (schedules:unwind_template_shift shift sid (lambda (shifts)
                (engine_job:shift_waiting sid shid (length shifts))
                  (tuple 'next_state 'waiting (maps:merge data
                    (map 's s 'unmatched_shifts shifts 'unmatched_employees '() 'rem_ack_match (length shifts) 'matched_shifts #M()
                      'shift (maps:put #"tags" tags shift)))))))))))))))

(defun waiting
  ;;
  ;; The job instructs to initiate the matching process
  ;; Here we just reply back to the job and we transition 
  ;; to the 'matching' state
  ;;
  (((tuple 'match emps) data) 
    (let ((sid (maps:get 'sid data))
          (shift (maps:get 'shift data))
          (data2 (maps:put 'total_employees emps data)))
      (engine_job:match_shift sid shift)
      (tuple 'next_state 'matching data2)))


  ;;
  ;; The job instructs to assign - we are working
  ;; with schedule shifts instead of template shifts
  ;; therefore we skip the matching and swapping phases
  (('assign data)
    (let ((sid (maps:get 'sid data))
          (shift (maps:get 'shift data)))
      (case (maps:get #"status" shift)
        (#"unassigned"
          (engine_job:shift_unassigned sid shift)
          (tuple 'next_state 'assigned data))
        (#"assigned"
          (engine_job:assign_shift sid shift)
          (tuple 'next_state 'assigning data))))))
    

(defun matching
  
  ;; 
  ;; A match request came from an employee. 
  ;; We map the employee to a shift id and 
  ;; we confirm the employee back
  ;;
  (((tuple 'query_match uid) data) 
    (let ((sid (maps:get 'sid data))
          (shid (maps:get 'shid data))
          (shift (maps:get 'shift data))
          (unmatched_shifts (maps:get 'unmatched_shifts data))
          (matched_shifts (maps:get 'matched_shifts data)))
      (case unmatched_shifts
        (() 
          (tuple 'next_state 'matching (kit:mladd 'unmatched_employees uid data)))

          ;;(let* ((data2 (kit:new_counters data (map 'match_replies 1)))
          ;;       (data3 (kit:mladd 'unmatched_employees uid data2))
          ;;       (match_replies (maps:get 'match_replies data3))
          ;;       (total_employees (maps:get 'total_employees data3)))
            ;;(case (== total_employees match_replies)
            ;;  ('false (tuple 'next_state 'matching data3))
            ;;  ('true 
            ;;    (engine_job:shift_matched sid shid 0)
            ;;    (tuple 'next_state 'matched data3)))))
        ((cons ushid rem)
          (let* ((data2 (maps:put 'unmatched_shifts rem data))
                 (data3 (maps:put 'matched_shifts (maps:put uid ushid matched_shifts) data2)))
            (engine_job_employee:grant_match sid uid shift)
            (tuple 'next_state 'matching data3))))))
  
  ;;
  ;; An employee process indicates it has 
  ;; no desire to match
  ;;
  (('pass_match data)
    (let* ((sid (maps:get 'sid data))
           (shid (maps:get 'shid data))
           (shift (maps:get 'shift data))
           (data2 (kit:new_counters data (map 'match_replies 1)))
           (total_employees (maps:get 'total_employees data2))
           (match_replies (maps:get 'match_replies data2)))
      (case (== match_replies total_employees)
        ('false 
          (tuple 'next_state 'matching data2))
        ('true 
          (engine_job:shift_matched sid shid 0)
          (tuple 'next_state 'matched data2)))))
  
  ;;
  ;; Employee did finish handling the match,
  ;; check if we are done matching.
  ;;
  (((tuple 'ack_match uid) data)
    (let* ((sid (maps:get 'sid data))
           (shid (maps:get 'shid data))
           (shift (maps:get 'shift data))
           (data2 (kit:new_counters data (map 'rem_ack_match -1 'match_replies 1)))
           (total_employees (maps:get 'total_employees data2))
           (match_replies (maps:get 'match_replies data2))
           (rem_ack_match (maps:get 'rem_ack_match data2)))
      (case rem_ack_match
        (0 
          (engine_job:shift_matched sid shid 1)
          (tuple 'next_state 'matched data2))
        (_ 
          (case (== match_replies total_employees)
            ('false 
              (engine_job:shift_matching sid shid)
              (tuple 'next_state 'matching data2))
            ('true 
              (engine_job:shift_matched sid shid 1)
              (tuple 'next_state 'matched data2))))))))
      

(defun matched

  ;;
  ;;  
  ;; 
  ;;(('pass_match data)
  ;;  (let* ((sid (maps:get 'sid data))
  ;;         (shid (maps:get 'shid data))
  ;;         (shift (maps:get 'shift data))
  ;;         (data2 (kit:new_counters data (map 'rem_ack_match -1 'match_replies 1)))
  ;;         (total_employees (maps:get 'total_employees data2))
  ;;         (match_replies (maps:get 'match_replies data2)))
  ;;    (tuple 'next_state 'matched data2)))

  ;;
  ;; The shift is already matched, but there are 
  ;; more employee processes that were willing to match
  ;; - we keep this info for the swapping phase
  ;;
  (((tuple 'query_match uid) data)
    (let ((data2 (kit:mladd 'unmatched_employees uid data)))
      (tuple 'next_state 'matched data2)))


  ;;
  ;; The shift was matched before all employees
  ;; had the time to reply
  ;;
  (('pass_match data)
    (tuple 'next_state 'matched data))
  ;; 
  ;; An employee initiator process is attempting to swap
  ;;
  (((tuple 'query_swap uid pickup) data)
    (let* ((sid (maps:get 'sid data))
          (shid (maps:get 'shid data))
          (shift (maps:get 'shift data))
          (matched_shifts (maps:get 'matched_shifts data))
          (peers (maps:size matched_shifts)))
      (engine_job_employee:setup_swap sid uid peers)
      (maps:map (lambda (peer _) (engine_job_employee:query_swap sid peer shift uid pickup)) matched_shifts)
      (tuple 'next_state 'swapping data)))

  ;;
  ;; Just ack back 
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'matched data)))
  
  ;; 
  ;; Perform a swap operation
  (((tuple 'swap from to) data)
    (do_swap from to data))
  
  ;; Checks a swap operation
  (((tuple 'check_swap from to) data)
    (do_check_swap from to data))

  ;;
  ;; The ongoing commit phase for a swapis aborted
  ;; One of the shifts is already in matched state
  ;; we just ignore this query
  (((tuple 'ack_swap _ _ _) data)
    (tuple 'next_state 'matched data)))

(defun swapping
  
  ;;
  ;; Interrupt the process - ack back and get ready
  ;; for the next phase
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'matched data)))

  ;;
  ;; The job process is instructing to make
  ;; a transfer of ownership
  ;;
  (((tuple 'swap from to) data)
    (do_swap from to data))
    
  ;; Checks a swap operation
  (((tuple 'check_swap from to) data)
    (do_check_swap from to data))
  
  ;;
  ;; Employee process confirms back with new quality deltas
  ;;
  (((tuple 'ack_swap uid hard soft) data)
    (let* ((sid (maps:get 'sid data))
           (shid (maps:get 'shid data))
           (data2 (kit:new_counters data (map 'hdelta hard 'sdelta soft 'swap_confirm_replies 1)))
           (swap_confirm_queries (maps:get 'swap_confirm_queries data2))
           (swap_confirm_replies (maps:get 'swap_confirm_replies data2)))
      (engine_notifier:emit 'employee_ack_swap sid (tuple uid hard soft))
      (case (== swap_confirm_replies swap_confirm_queries)
        ('false (tuple 'next_state 'swapping data2))
        ('true 
          (engine_job:ack_swap sid shid (maps:get 'stdelta data2) (maps:get 'hdelta data2) (maps:get 'sdelta data2))
          (tuple 'next_state 'swapping (maps:merge data2 (map 'hdelta 0 'sdelta 0 'swap_confirm_replies 0 'swap_confirm_queries 0))))))))

;;
;; All state events, such as errors and timeouts and 
;; the stop command
;;
(defun handle_event
  
  ;;
  ;; clean stop -  we persist our employee vs shift id mappings
  ;; and we notify back
  ;;
  (('stop state data)
    (let ((sid (maps:get 'sid data))
          (s (maps:get 's data))
          (shid (maps:get 'shid data)))
      (case (maps:get #"auto" s)
        (#"false" 
          (engine_job:shift_stopped sid shid)
          (tuple 'next_state 'end data))
        (#"true"
          (let ((matched_shifts (maps:get 'matched_shifts data)))
            (schedules:with_schedule sid (lambda (s)
              (lists:map (match-lambda 
                (((tuple uid shid))
                  (schedules:with_schedule_shift s shid (lambda (shift)
                    (schedules:assign_shift s shift uid))))) (maps:to_list matched_shifts))
                  (engine_job:shift_stopped sid shid)
                  (tuple 'next_state 'end data)))))))))

;;
;; Here we just ignore everything
;;
(defun end (_ data)
  (tuple 'next_state 'end data))

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
