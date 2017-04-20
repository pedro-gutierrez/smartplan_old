(defmodule engine_job
  (behavior gen_fsm)
  (export all))

(defun start_link (id)
  (gen_fsm:start_link 'engine_job (list id) '()))

(defun create (id) 
  (engine_jobs_sup:create id))

(defun run (id) 
  (engine_registry:with 'job id (lambda (pid) 
    (gen_fsm:send_event pid 'run))))

(defun lookup (sid)
  (engine_registry:with 'job sid (lambda (pid) pid)))

(defun exists? (sid)
  (engine_registry:exists? 'job sid))

(defun send (sid e)
  (gen_fsm:send_event (lookup sid) e))

;;
;; remove all processes related
;; to the specified job
;;
(defun delete (id)
  (engine_jobs_sup:delete id)
  (schedules:with_schedule id (lambda (s)
    (case (maps:get #"status" s)
      (#"running" (schedules:set_schedule_status s #"error" (lambda (_) ))) 
      (_ )))))

;;
;; clean stop of a job
;;
(defun stop (sid)
  (gen_fsm:send_all_state_event 
    (engine_registry:lookup 'job sid) 'stop ))

;; starts the debriefing phase of a job
(defun debrief (sid)
  (send sid 'debrief))

;; a week process finished debriefing
(defun debriefed (sid)
  (send sid 'debriefed)) 

(defun running ()
  (common:count_status #"schedules" #"running" (lambda (r) r)))

(defun enqueue (id)
  (common:queue #"schedules" id))

(defun dequeue ()
  (common:dequeue #"schedules"))

(defun unqueue (id)
  (common:unqueue #"schedules" id))

(defun mark (id status)
  (schedules:with_schedule id (lambda (s)
    (schedules:set_schedule_status s status (lambda (s2) s2)))))

(defun create_employee (sid o u) 
  (engine_job_employees_sup:create sid o u))

(defun create_shift (sid shift) 
  (engine_job_shifts_sup:create sid shift))


(defun create_week (sid u y w)
  (engine_job_weeks_sup:create sid u y w))

;;
;; Same as 'send', but using gen_fsm:send_all_event
;;
(defun send_all (sid e)
  (gen_fsm:send_all_state_event (lookup sid) e))


(defun shift_waiting (sid shid staffing)
  (send sid (tuple 'shift_waiting staffing)))

;;
;; Called by employee processes to notify
;; their participation in the matching and swap phases
;;
(defun employee_waiting (sid uid)
  (send sid 'employee_waiting ))

(defun match (sid)
  (send sid 'match))


(defun start (sid)
  (send sid 'start))

(defun match_shift (sid shift)
  (send sid (tuple 'match_shift shift)))

;;
;; This function is called by employee 
;; processes that are matching (whether they are 
;; granted or not) - it provides the total number
;; of hard and soft contraints for that employee
;;
(defun employee_matching (sid uid hard soft) 
  (send sid (tuple 'employee_matching uid hard soft)))

;;
;; This function is called by shift processes
;; when they have been partially matched
;;
(defun shift_matching (sid shid)
  (send sid (tuple 'shift_matching shid)))

;;
;; This function is called by shift processes
;; whenver they have been completely matched
;;
(defun shift_matched (sid shid st)
  (send sid (tuple 'shift_matched shid st)))

;;
;; This function is called by a shift process
;; when it has finished stopping
;;
(defun shift_stopped (sid shid)
  (send sid 'child_stopped))

;;
;; This function is called by an employee process
;; when it has been assigned a shift
;;
(defun shift_assigned (sid shift dh ds)
  (send sid (tuple 'shift_assignment shift 'true dh 0 ds 0)))

;;
;; This function is called by an employee process
;; the very first time it has been assigned a shift
;;
(defun shift_assigned (sid shift dh h ds s)
  (send sid (tuple 'shift_assignment shift 'true dh h ds s)))

;;
;; This function is called by a shift process
;; when the shift is not assigned
;;
(defun shift_unassigned (sid shift)
  (send sid (tuple 'shift_assignment shift 'false 0 0 0 0)))

;;
;; This function is called by a shift process
;; when it needs to tell the job to start the assignment
;; phase
(defun assign_shift (sid shift)
  (send sid (tuple 'assign_shift shift)))

;;
;; This function is called by a shift process
;; when it has finished stopping
;;
(defun employee_stopped (sid uid)
  (send sid 'child_stopped))

;; This function is called by employee
;; processes once their quality has changed
;; because of a match or a swap. Parameters are
;; net gains (positive or negative) in terms of satisfied
;; hard and soft constraints
;;
(defun quality (sid hard soft)
  (send_all sid (tuple 'quality hard soft)))

;; 
;; This function is called by either shift or 
;; employee processes whenever an unexpected error
;; occurs 
;;
(defun error (sid)
  (send_all sid 'error))

;;
;; Computes the quality as a percentage of 
;; number of contraints satisfied over the total number
;; of constraints
;;
(defun quality (dt d ht h st s)
  (trunc (* 100 (/ (+ d h s) (+ dt ht st)))))

;;
;; Convenience function that returns the total quality 
;; given the fsm's data
(defun set_quality (data)
  (let* ((staffing_total (maps:get 'staffing_total data))
        (staffing (maps:get 'staffing data))
        (hard_total (maps:get 'hard_total data))
        (hard (maps:get 'hard data))
        (soft_total (maps:get 'soft_total data))
        (soft (maps:get 'soft data))
        (q (quality staffing_total staffing hard_total hard soft_total soft)))
    (maps:put 'quality q data)))

;; 
;; Returns a tuple containing all the quality related
;; info (staffing + hard and soft constraints)
;;
(defun quality (data)
  (let ((staffing_total (maps:get 'staffing_total data))
        (staffing (maps:get 'staffing data))
        (hard_total (maps:get 'hard_total data))
        (hard (maps:get 'hard data))
        (soft_total (maps:get 'soft_total data))
        (soft (maps:get 'soft data)))
    (tuple 'staffing_total staffing_total 'staffing staffing
           'hard_total hard_total 'hard hard
           'soft_total soft_total 'soft soft)))

(defun compare_employee_quality 
  (('undefined e2 next) (funcall next 1 1))
  ((e1 'undefined next) (funcall next -1 -1))
  ((e1 e2 next)
    (let* (((tuple _ h1 s1) e1)
           ((tuple _ h2 s2) e2)
           (hdiff (- h1 h2))
           (sdiff (- s1 s2)))
      (funcall next (kit:sign hdiff) (kit:sign sdiff))))) 

(defun update_swap_initiator (new_init data next)
    (let ((old_init (maps:get 'swap_initiator data)))
      (compare_employee_quality new_init old_init (match-lambda
        ((-1 _) (funcall next (maps:put 'swap_initiator new_init data)))
        ((0 -1) (funcall next (maps:put 'swap_initiator new_init data)))
        ((_ _) (funcall next data)))))) 

;; Start a swap phase
(defun swap (sid)
  (send sid 'swap))

(defun swap (sid uid h s)
  (send sid (tuple 'swap uid h s)))

(defun swap (sid uid)
  (send sid (tuple 'swap uid 'pass)))

(defun ack_grant_swap (sid uid)
  (send sid (tuple 'ack_grant_swap uid)))

(defun ack_deny_swap (sid uid)
  (send sid (tuple 'ack_deny_swap uid)))

(defun abort_swap (sid)
  (send sid 'abort_swap))

(defun no_swap_found (sid)
  (send sid 'no_swap_found))

(defun ack_finish_swap (sid)
  (send sid 'ack_finish_swap))

;;
;; This function is called by the initiator employee 
;; process once a swap handshake has been retained
;;
(defun commit_swap 
  ((sid (tuple ouid pshift puid))(send sid (tuple 'commit_swap ouid pshift puid )))
  ((sid (tuple oshift ouid pshift puid))(send sid (tuple 'commit_swap oshift ouid pshift puid))))

(defun swap_check_ok (sid shid from to) 
  (send sid (tuple 'swap_check_ok shid from to)))

(defun swap_check_nok (sid shid from to) 
  (send sid (tuple 'swap_check_nok shid from to)))

;;
;; This function is called by shift processes
;; as they finish applying shift transfers
;;
(defun ack_swap (sid shid st h s)
  (send sid (tuple 'ack_swap shid st h s)))

;;
;; Retains a swap transaction, after it has been verified
;;
(defun retain_swap_transaction (shid from to data)
  (kit:mladd 'swap_transactions_retained (tuple shid from to) data))

;;
;; Applies retained swaps on each affected shift
;;
(defun apply_retained_swaps (data)
  (let ((sid (maps:get 'sid data))
         (retained_swaps (maps:get 'swap_transactions_retained data)))
    (engine_notifier:emit 'swap_commit sid)
      (lists:map (match-lambda (((tuple shid from to)) 
        (engine_job_shift:swap shid from to))) retained_swaps)
    (tuple 'next_state 'committing_swap data)))

;;
;; After all employee processes have applied for a swap
;; prepare the responses (1 grant, n denials) or notify
;; the planner if no initiator did apply
;;
(defun send_swap_application_responses (data)
  (let* ((sid (maps:get 'sid data))
         (initiator (maps:get 'swap_initiator data))
         (employees (maps:get 'employees data))
         (initiators (maps:get 'initiators data)))
    (case initiator 
      ('undefined
        (engine_notifier:emit 'no_swap_initiator sid)
        (tuple 'next_state 'matched data))
      ((tuple initiator _ _)
        (let ((non_initiators (lists:filter (lambda (uid) (!= uid initiator)) initiators)))
          (lists:map (lambda (uid) (engine_job_employee:deny_swap sid uid)) non_initiators)
          (engine_job_employee:grant_swap sid initiator)
          (engine_notifier:emit 'swapping sid)
          (tuple 'next_state 'swapping data))))))

(defun finish_swap (data)
  (let* ((sid (maps:get 'sid data))
         (shifts (maps:get 'shifts data))
         (employees (maps:get 'employees data))
         (data2 (maps:put 'swap_finish_queries (+ (length employees) (length shifts)) data)))
    (engine_notifier:emit 'finishing_swap sid)
    (lists:map (lambda (uid) (engine_job_employee:finish_swap sid uid)) employees)
    (lists:map (lambda (shid) (engine_job_shift:finish_swap shid)) shifts)
    (tuple 'next_state 'finishing_swap data2)))

(defun setup_schedule_job (o sid y w shifts employees next)
  (lists:map (lambda (shift) (create_shift sid shift)) shifts)
  (lists:map (lambda (u) (create_employee sid o u)) employees)
  (lists:map (lambda (u) (create_week sid u y w)) employees)
  (funcall next))
  
(defun set_schedule_running (s shifts employees data next)
  (let* ((eids (lists:map (lambda (e) (maps:get #"id" e)) employees))
         (data2 (maps:merge data 
            (map 'rem_init (+ (length employees) (length shifts))
              'rem_stop (+ (length employees) (length shifts))
              'shifts (lists:map (lambda (shift) (maps:get #"id" shift)) shifts)
              'employees eids
              'initiators '() 'employees_matching '()))))
    (case (or (== 0 (length employees)) (== 0 (length shifts) ))
      ('true 
        (let ((sid (maps:get 'sid data)))
          (engine_notifier:emit 'not_enough_data sid)
          (tuple 'next_state 'finished data2))) 
      ('false
        (schedules:set_schedule_status s #"running" (lambda (s2)
          (funcall next (maps:put 's s2 data2))))))))

(defun init (((list id))
  (engine_registry:register 'job id)
  (tuple 'ok 'starting (map 'sid id) 0)))

(defun starting
  (('timeout data)
    (let ((id (maps:get 'sid data)))
      (schedules:with_schedule id (lambda (s)
        (organizations:with_organization (maps:get #"organization" s) (lambda (o)
          (schedules:set_schedule_status s #"queued" (lambda (s2)
            (enqueue id)
            (engine_notifier:emit 'waiting id)
            (tuple 'next_state 'waiting 
              (maps:merge (map 'quality 0
                               'hard 0 'hard_total 0 
                               'soft 0 'soft_total 0 
                               'staffing 0 'staffing_total 0 'o o 's s 'swap_commit_replies 0) data )))))))))))

(defun waiting
  
  ;;
  ;; The planner instructed to start running the calculation
  ;; We will initialize all children processes and keep 
  ;; waiting until they are all ready
  ;;
  (('run data)
    (let* ((s (maps:get 's data))
           (y (maps:get #"year" s))
           (w (maps:get #"week" s))
           (o (maps:get 'o data))
           (sid (maps:get 'sid data))
           (oid (maps:get #"id" o)))
      (case (maps:get #"auto" s)
        (#"false" 
          (schedules:with_schedule_shifts s (lambda (shifts)
            (schedules:with_schedule_participants shifts (lambda (employees)
              (schedules:clear_constraints s (lambda()
                (setup_schedule_job o sid y w shifts employees (lambda () 
                  (set_schedule_running s shifts employees data (lambda (data2)
                    (tuple 'next_state 'waiting data2))))))))))))
        (#"true"
          (schedules:with_schedule_template_shifts s (lambda (shifts)
            (organizations:with_members o (lambda (employees)
              (schedules:clear_schedule s (lambda ()
                (setup_schedule_job o sid y w shifts employees (lambda () 
                  (set_schedule_running s shifts employees data (lambda (data2)
                    (tuple 'next_state 'waiting data2)))))))))))))))
          
  (('employee_waiting data)
    (let* ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'rem_init -1))))
      (case (maps:get 'rem_init data2)
        (0 
          (engine_notifier:emit 'ready sid)
          (tuple 'next_state 'ready data2))
        (_ (tuple 'next_state 'waiting data2)))))
      
  (((tuple 'shift_waiting staffing) data)
    (let* ((sid (maps:get 'sid data))
          (counters (map 'staffing_total staffing 'rem_init -1))
          (data2 (kit:new_counters data counters)))
      (case (maps:get 'rem_init data2)
        (0 
          (engine_notifier:emit 'ready sid)
          (tuple 'next_state 'ready data2))
        (_ (tuple 'next_state 'waiting data2))))))


(defun ready

  (('start data)
    (let ((sid (maps:get 'sid data))
          (s (maps:get 's data))
          (shifts (maps:get 'shifts data))
          (emps (length (maps:get 'employees data))))
      (case (maps:get #"auto" s)
        (#"false" 
          (lists:map (lambda (shid) (engine_job_shift:assign shid)) shifts)
          (tuple 'next_state 'assigning (maps:put 'rem_assigning 0 data)))
        (#"true" 
            (lists:map (lambda (shid) (engine_job_shift:match shid emps)) shifts)
            (tuple 'next_state 'matching data ))))))



(defun assigning
  
  ;;
  ;; Contact the employee and send the shift for assignment
  ;;
  (((tuple 'assign_shift shift) data)
    (let ((sid (maps:get 'sid data)))
      (engine_job_employee:assign sid (maps:get #"assignedto" shift) shift)
      (tuple 'next_state 'assigning data)))
  
  ;;
  ;; A shift has been assigned
  ;;
  (((tuple 'shift_assignment shid res dh h ds s) data)
    (let* ((sid (maps:get 'sid data))
           (shifts (maps:get 'shifts data))
           (data2 (kit:new_counters data (map 'rem_assigning 1 'hard dh 'hard_total h 'soft ds 'soft_total s)))
           (data3 (case res ('true (kit:new_counters data2 (map 'staffing 1)))('false data2)))
           (data4 (set_quality data3))
           (rem_assigning (maps:get 'rem_assigning data4)))
      (case (== (length shifts) rem_assigning)
        ('false (tuple 'next_state 'assigning data4))
        ('true 
          (engine_notifier:emit 'assigned sid (quality data4))
          (tuple 'next_state 'assigned data4))))))
  

(defun matching
  
  ;;
  ;; A shift responsed ready to the matching
  ;; phase. The job broadcasts the shift data to
  ;; every employee
  ;;
  (((tuple 'match_shift shift) data)
    (let ((sid (maps:get 'sid data))
          (employees (maps:get 'employees data)))
      (lists:map (lambda (eid) (engine_job_employee:match sid eid shift)) employees)
      (tuple 'next_state 'matching data)))

  ;;
  ;; An employee is attempting to match shifts 
  ;; - we keep track of the total number of constraints
  ;; for that employee
  ;;
  (((tuple 'employee_matching uid hard soft) data)
    (let* ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'hard_total hard 'soft_total soft)))
          (data3 (kit:mladd 'employees_matching uid data2))
          (data4 (kit:mladd 'initiators uid data3)))
      (engine_notifier:emit 'employee_matching sid uid)
      (tuple 'next_state 'matching data4)))

  ;;
  ;; A shift is partially matched - we increase the
  ;; current staffing by one
  ;; 
  (((tuple 'shift_matching shid) data)
    (let* ((s (maps:get 's data))
          (sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'staffing 1)))
          (data3 (set_quality data2)))
      (tuple 'next_state 'matching data3)))

  ;;
  ;; A shift has been fully matched. Check whether this is 
  ;; the end of the matching phase
  ;;
  (((tuple 'shift_matched shid st) data)
    (let* ((s (maps:get 's data))
           (sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'staffing st 'matched_shifts 1)))
           (data3 (set_quality data2))
           (staffing (maps:get 'staffing data3))
           (staffing_total (maps:get 'staffing_total data3))
           (hard (maps:get 'hard data3))
           (hard_total (maps:get 'hard_total data3))
           (soft (maps:get 'soft data3))
           (soft_total (maps:get 'soft_total data3))
           (matched_shifts (maps:get 'matched_shifts data3))
           (shifts (maps:get 'shifts data3)))
      (case (== matched_shifts (length shifts))
        ('true 
          (case (maps:get 'quality data3) 
            (100
                (engine_notifier:emit 'finished sid)
                (tuple 'next_state 'finished (maps:put 's s data3)))
            (_
              (lists:map (lambda (uid) (engine_job_employee:matched sid uid))
                (maps:get 'employees_matching data))
              (engine_notifier:emit 'matched sid (quality data3))
              (tuple 'next_state 'matched data3))))
        ('false (tuple 'next_state 'matching data3))))))

(defun matched

  ;;
  ;; An employee is attempting to match shifts 
  ;; - we keep track of the total number of constraints
  ;; for that employee
  ;;
  (((tuple 'employee_matching uid hard soft) data)
    (let* ((sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'hard_total hard 'soft_total soft)))
           (data3 (kit:mladd 'employees_matching uid data2))
           (data4 (kit:mladd 'initiators uid data3)))
      (engine_notifier:emit 'employee_matching sid uid)
      (tuple 'next_state 'matched data4)))


  ;; The planner instructs to start a new swap
  ;; phase. In the special case we have only one employee, 
  ;; we just go to finished state
  (('swap data)
    (let ((sid (maps:get 'sid data))
          (initiators (maps:get 'initiators data))
          (matching_emps (maps:get 'employees_matching data)))
      (case (length matching_emps)
        (1 
          (engine_notifier:emit 'finished sid)
          (tuple 'next_state 'finished data))
        (_
          (lists:map (lambda (uid) 
            (engine_job_employee:swap sid uid)) initiators)
          (tuple 'next_state 'swapping 
            (maps:put 'swap_applications 0 data)))))))


(defun swapping
  
  ;; 
  ;; An employee applies as the initiator for
  ;; the current swap phase
  (((tuple 'swap uid hard soft) data)
    (let* ((sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'swap_applications 1)))
           (swap_applications (maps:get 'swap_applications data2))
           (total_initiators (length (maps:get 'initiators data2))))
      (engine_notifier:emit 'swap_application sid uid)
      (case swap_applications
        (1
          (let ((data3 (maps:put 'swap_initiator (tuple uid hard soft) data2)))
            (case (== swap_applications total_initiators)
              ('false (tuple 'next_state 'swapping data3)) 
              ('true (send_swap_application_responses data3)))))
        (_ 
          (update_swap_initiator (tuple uid hard soft) data2 (lambda (data3)
            (case (== swap_applications total_initiators)
              ('false (tuple 'next_state 'swapping data3)) 
              ('true (send_swap_application_responses data3)))))))))

   
  ;;
  ;; An employee process decides not to apply
  ;; as initiator in the current swap phase
  (((tuple 'swap uid 'pass) data)
    (let* ((sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'swap_applications 1)))
           (swap_applications (maps:get 'swap_applications data2))
           (total_initiators (length (maps:get 'initiators data2))))
      (engine_notifier:emit 'swap_pass sid uid)
      (case swap_applications
        (1 
          (let ((data3 (maps:put 'swap_initiator 'undefined data2)))
            (case (== swap_applications total_initiators)
              ('false (tuple 'next_state 'swapping data3)) 
              ('true (send_swap_application_responses data3)))))
        (_ 
          (case (== swap_applications total_initiators)
            ('false (tuple 'next_state 'swapping data2))
            ('true (send_swap_application_responses data2)))))))
              

  ;;
  ;; The initiator process did ack the swap grant
  ;;
  (((tuple 'ack_grant_swap uid) data)
    (let* ((sid (maps:get 'sid data))
           (initiators (maps:get 'initiators data))
           (rem_initiators (lists:filter (lambda (eid) (=/= uid eid)) initiators))
           (new_initiators (case rem_initiators
                              ('() (maps:get 'employees_matching data))
                              (_ rem_initiators))))
      (engine_notifier:emit 'ack_grant_swap sid uid)
      (tuple 'next_state 'swapping (maps:put 'initiators new_initiators data))))

  
  ;;
  ;; A peer process did ack the swap denial
  ;;
  (((tuple 'ack_deny_swap uid) data)
    (let ((sid (maps:get 'sid data)))
      (engine_notifier:emit 'ack_deny_swap sid uid)
      (tuple 'next_state 'swapping data)))
  

  ;;
  ;; Commit a shift transfer from a peer to the originator
  ;;
  (((tuple 'commit_swap ouid pshift puid) data)
    (let ((sid (maps:get 'sid data))
          (shid (maps:get #"id" pshift)))
      (engine_job_shift:check_swap shid puid ouid)
      (engine_notifier:emit 'swap_check sid)
      (tuple 'next_state 'checking_swap (maps:merge data 
        (map 'consecutive_failed_swap_attempts 0 
             'swap_transactions_retained '()
             'swap_transactions (list (tuple shid puid ouid)))))))

  ;;
  ;; Commit a shift swap between peer and the originator
  ;;
  (((tuple 'commit_swap oshift ouid pshift puid) data)
    (let ((sid (maps:get 'sid data))
          (pshid (maps:get #"id" pshift))
          (oshid (maps:get #"id" oshift)))
      (engine_notifier:emit 'swap_check sid)
      (engine_job_shift:check_swap pshid puid ouid)
      (engine_job_shift:check_swap oshid ouid puid)
      (tuple 'next_state 'checking_swap (maps:merge data 
        (map 'consecutive_failed_swap_attempts 0 
             'swap_transactions_retained '()
             'swap_transactions (list (tuple pshid puid ouid) (tuple oshid ouid puid)))))))

  ;;
  ;; The initiator informs no swap was found 
  ;;
  (('no_swap_found data)
    (let* ((sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'consecutive_failed_swap_attempts 1))))
      (engine_notifier:emit 'no_swap_handshakes sid)
      (finish_swap data2)))

  ;;
  ;; The initiator process aborted the swap grant
  ;;
  (('abort_swap data)
    (finish_swap data)))

(defun checking_swap

  ;;
  ;; A shift reported the swap operation is ok to be 
  ;; committed
  ;;
  (((tuple 'swap_check_ok shid from to) data)
    (let* ((sid (maps:get 'sid data))
           (data2 (kit:new_counters data (map 'swap_check_replies 1)))
           (swap_check_replies (maps:get 'swap_check_replies data2))
           (swap_transactions (maps:get 'swap_transactions data2))
           (data3 (retain_swap_transaction shid from to data2))
           (retained_swaps (maps:get 'swap_transactions_retained data3)))
      (engine_notifier:emit 'swap_check_ok sid)
      (case (== (length swap_transactions) swap_check_replies)
        ('true
          (case (length retained_swaps)
            (0 (finish_swap data3))
            (_ (apply_retained_swaps data3)))) 
        ('false 
          (tuple 'next_state 'checking_swap data3)))))
  
  ;; 
  ;; A shift reported the swap operation is not ok to be 
  ;; committed
  ;;
  (((tuple 'swap_check_nok shid from to) data)
    (let* ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'swap_check_replies 1)))
          (swap_check_replies (maps:get 'swap_check_replies data2))
          (swap_transactions (maps:get 'swap_transactions data2))
          (retained_swaps (maps:get 'swap_transactions_retained data2)))
      (engine_notifier:emit 'swap_check_nok sid)
      (case (== (length swap_transactions) swap_check_replies)
        ('true
          (case (length retained_swaps)
            (0 (finish_swap data2))
            (_ (apply_retained_swaps data2)))) 
        ('false
          (tuple 'next_state 'checking_swap data2))))))

(defun committing_swap

  ;;
  ;; A shift process finished doing the transfer of shifts between
  ;; two shift processes
  ;;
  (((tuple 'ack_swap shid staffing hard soft) data)
    (let* ((data2 (kit:new_counters data (map 'swap_commit_replies 1 'staffing staffing 'hard hard 'soft soft)))
           (data3 (set_quality data2))
           (swap_commit_replies (maps:get 'swap_commit_replies data3))
           (swap_transactions_retained (maps:get 'swap_transactions_retained data3))
           (sid (maps:get 'sid data3)))
      (engine_notifier:emit 'shift_ack_swap sid (tuple shid hard soft))
      (case (== (length swap_transactions_retained) swap_commit_replies)
        ('false (tuple 'next_state 'committing_swap data2))
        ('true 
          (engine_notifier:emit 'swapped sid (quality data3))
          (finish_swap data3))))))

(defun finishing_swap 
  
  ;;
  ;; We still receive swap check replies but the 
  ;; swap has been already interrupted
  ;;
  (((tuple 'swap_check_ok _ _ _) data)
    (tuple 'next_state 'finishing_swap data))

  ;;
  ;; We still receive swap check replies but the 
  ;; swap has been already interrupted
  ;;
  (((tuple 'swap_check_nok _ _ _) data)
    (tuple 'next_state 'finishing_swap data))

  ;;
  ;; A shift or employee process acks back when the swap phase
  ;; is being aborted
  ;;
  (('ack_finish_swap data)
    (let* ((data2 (kit:new_counters data (map 'swap_finish_replies 1)))
           (swap_finish_replies (maps:get 'swap_finish_replies data2))
           (swap_finish_queries (maps:get 'swap_finish_queries data2))
           (sid (maps:get 'sid data)))
      (case (== swap_finish_replies swap_finish_queries)
        ('false (tuple 'next_state 'finishing_swap data2))
        ('true 
          (case (maps:get 'quality data2) 
            (100
              (engine_notifier:emit 'finished sid)
              (tuple 'next_state 'finished data2))
            (_    
              (let ((cfsa (maps:get 'consecutive_failed_swap_attempts data2))
                    (employees (maps:get 'employees data2))
                    (shifts (maps:get 'shifts data2)))
                (case (== cfsa (* 2 (length shifts)))
                  ('false 
                    (engine_notifier:emit 'swap_finished sid)
                    (tuple 'next_state 'matched (maps:merge data2 
                      (map 'swap_finish_queries 0 'swap_finish_replies 0 
                           'swap_transactions '() 'swap_transactions_retained '() 
                           'swap_check_replies 0 'swap_commit_replies 0))))
                  ('true
                    (engine_notifier:emit 'finished sid)
                    (tuple 'next_state 'finished data2)))))))))))
                    


(defun finished
  
  ;;
  ;; This happens when the job finishes early, because
  ;; there are no employees to be initialized 
  ;; (unlikely, cos there is always one)
  ;;
  (((tuple 'shift_waiting _) data)
    (tuple 'next_state 'finished data))

  ;; This happens when the job finishes early, because
  ;; there are no shifts to be initialized.
  ;;
  (('employee_waiting data)
    (tuple 'next_state 'finished data)))

(defun stopping
 
  ;;
  ;; a child process stopped - keep looking
  ;; or notify the planner
  ;;
  (('child_stopped data)
    (let ((data2 (kit:new_counters data (map 'rem_stop -1))))
      (case (maps:get 'rem_stop data2)
        (0
          (engine_notifier:emit 'stopped (maps:get 'sid data2))
          (tuple 'next_state 'stopped data2))
        (_ (tuple 'next_state 'stopping data2)))))
  
  ((_ data) (tuple 'next_state 'stopping data)))

(defun stopped
  
  ;;
  ;; the planner starts the debriefing phase
  ;;
  (('debrief data)
    (let* ((sid (maps:get 'sid data))
          (s (maps:get 's data))
          (employees (maps:get 'employees data))
          (y (maps:get #"year" s))
          (w (maps:get #"week" s))
          (data2 (maps:put 'rem_debriefing (length employees) data))
          (rem_debriefing (maps:get 'rem_debriefing data2)))
        (case rem_debriefing 
          (0
            (schedules:set_schedule_status s #"computed" (lambda (s2)
              (engine_notifier:emit 'debriefed sid)
                (tuple 'next_state 'debriefed data2))))
          (_
            (lists:map (lambda (uid) (engine_job_week:debrief sid uid y w) ) employees)
            (tuple 'next_state 'debriefing data2))))))

(defun debriefing
  
  ;;
  ;; A week process finished debriefing
  ;;
  (('debriefed data)
    (let* ((sid (maps:get 'sid data))
           (s (maps:get 's data))
           (rem_debriefing (maps:get 'rem_debriefing data)))
      (case rem_debriefing
        (1
          (schedules:set_schedule_status s #"computed" (lambda (s2)
            (engine_notifier:emit 'debriefed sid)
            (tuple 'next_state 'debriefed (maps:put 'rem_debriefing 0 (maps:put 's s2 data))))))
        (_ (tuple 'next_state 'debriefing (maps:put 'rem_debriefing (- rem_debriefing 1)  data)))))))

;; All state events, such as errors, timeouts and 
;; the stop command, 
;;
(defun handle_event
  
  ;;
  ;; Quality reports can happen while matching 
  ;; or swapping
  ;;
  (((tuple 'quality hard soft) state data)
    (let* ((data2 (kit:new_counters data (map 'hard hard 'soft soft)))
          (data3 (set_quality data2)))
      (tuple 'next_state state data3)))

  ;;
  ;; clean stop of all children processes
  ;;
  (('stop state data)
    (let ((sid (maps:get 'sid data))
          (s (maps:get 's data))
          (shifts (maps:get 'shifts data))
          (employees (maps:get 'employees data)))
        (schedules:set_schedule_stats s 
          (map #"quality" (maps:get 'quality data)
             #"staffing" (maps:get 'staffing data) 
             #"staffing_total" (maps:get 'staffing_total data)
             #"hard" (maps:get 'hard data) 
             #"hard_total" (maps:get 'hard_total data)
             #"soft" (maps:get 'soft data)
             #"soft_total" (maps:get 'soft_total data)) (lambda (_)
            (case (or (> (length shifts) 0) (> (length employees) 0))
              ('false 
                (engine_notifier:emit 'stopped sid)
                (tuple 'next_state 'stopped data))
              ('true
                (lists:map (lambda (shift) (engine_job_shift:stop shift)) shifts)
                (lists:map (lambda (u) (engine_job_employee:stop sid u)) employees)
                (tuple 'next_state 'stopping data)))))))
  
  ;;
  ;; update the schedule status 
  ;;
  (('error state data) 
    (let ((sid (maps:get 'sid data))
          (s (maps:get 's data)))
      (schedules:set_schedule_status s #"error" (lambda (_)
        (engine_notifier:emit 'error sid )
        (tuple 'next_state 'ended data))))))

;;
;; Shutdown of the state machine. We need to distinguish
;; different cases, in particular, if this is a local failure,
;; inform the job process so that we clean up the schedule
;; state
;;
(defun terminate 
  (('normal _ _))
  ((_ _ data)
    (let ((sid (maps:get 'sid data))
          (s (maps:get 's data)))
      (schedules:set_schedule_status s #"error" (lambda (_)
        (engine_notifier:emit 'error sid))))))
