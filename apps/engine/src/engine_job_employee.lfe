(defmodule engine_job_employee
  (behavior gen_fsm)
  (export all))

(defun start_link (sid o u)
  (gen_fsm:start_link 'engine_job_employee (list sid o u) '()))

(defun register (sid u)
  (engine_registry:register 'employee (kit:catbin (list sid (maps:get #"id" u)))))

(defun lookup (sid uid)
  (engine_registry:lookup 'employee (kit:catbin (list sid uid))))

(defun send (sid uid e)
  (gen_fsm:send_event (lookup sid uid) e))

(defun match (sid uid shift)
  (send sid uid (tuple 'match shift)))

(defun swap (sid uid)
  (send sid uid 'swap)) 

(defun assign (sid uid shift)
  (send sid uid (tuple 'assign shift)))

;;
;; clean stop of an employee process
;;
(defun stop (sid uid)
  (gen_fsm:send_all_state_event (lookup sid uid) 'stop))

;; This function is called by a shift process
;; to indicate a match query was granted
;;
(defun grant_match (sid uid shift)
  (send sid uid (tuple 'grant_match shift)))

;;
;; This function is called by the job process
;; as soon as the matching phased is finished
;; 
(defun matched (sid uid)
  (send sid uid 'matched))


(defun grant_swap (sid uid)
  (send sid uid 'grant_swap))

(defun deny_swap (sid uid)
  (send sid uid 'deny_swap))

(defun finish_swap (sid uid)
  (send sid uid 'finish_swap))

;;
;; Computes the new stats for the employee - returns a tuple 
;; in the form {deltaHc, deltaSc, new_data}
;;
(defun update_stats (data)
  (let ((matched_shifts (maps:get 'matched_shifts data)))
    (eval_rules matched_shifts data (lambda (h s v stats ver nver)
      (with_quality_delta h s v data (lambda (dh ds dv)
        (tuple dh ds dv (maps:merge data (map 'h_rules_verified h 's_rules_verified s 'rule_violations v 
                                            'stats stats 'rules_verified ver 'rules_non_verified nver))))))))) 

(defun quality_delta (h s v data)
  (with_quality_delta h s v data (lambda (dh ds dv)
    (tuple dh ds dv))))

;;  
;; Computes the quality delta in
;; in terms of delta of hard, soft verified rules
;; and rule violations
;;
(defun with_quality_delta (h s v data next)
  (let* ((h1 (maps:get 'h_rules_verified data))
         (s1 (maps:get 's_rules_verified data))
         (v1 (maps:get 'rule_violations data))
         (dh (- h h1))
         (ds (- s s1))
         (dv (- v v1)))
    (funcall next dh ds dv)))

;;
;; Runs all rules against the given set of shifts 
;; and yields the total number of verified hard and soft constraints
;;
(defun eval_rules (shifts data next) 
  (let* ((rules (maps:get 'rules data))
         (u (maps:get 'u data))
         (s (maps:get 's data))
         (y (kit:bin2int (maps:get #"year" s)))
         (w (kit:bin2int (maps:get #"week" s)))
         (stats (maps:get 'stats_before data)))
      (let (((tuple h s v stats2 ver nver) (schedules:eval_rules rules shifts stats)))
        (funcall next h s v stats2 ver nver))))

(defun match_shifts (shifts data)
  (lists:filter (lambda (s) 
    (match? s data)) shifts))

(defun match? (shift data)
  (match_tags? shift data (lambda ()
    (match_availability? shift data (lambda () 'true)))))


(defun set_availability_shifts (u a stats next)
  (case (maps:get #"status" a)
    (#"disabled" (funcall next 'undefined stats))
    (_ 
      (schedules:with_availability_shifts u a (lambda (shifts)
        (schedules:with_shifts_duration shifts (lambda (d)
          (funcall next (schedules:shifts_by_day shifts) (maps:put 'av_time_w d stats)))))))))

;;
;; Checks whether or not the given shift matches
;; based on employee availability
;;
(defun match_availability? (shift data next)
  (let ((shifts_d (maps:get 'avshifts data)))
    (case shifts_d
      ('undefined (funcall next))
      (_ 
        (let ((d (maps:get #"start_day" shift)))
          (case (maps:is_key d shifts_d)
            ('false 'false)
            ('true (schedules:shift_within_shifts? shift (maps:get d shifts_d)))))))))
          
;;
;; Checks whether or not the given shift 
;; matches based on both shift and employee tags
;;
(defun match_tags? (shift data next) 
  (let ((stags (maps:get #"tags" shift))
        (etags (maps:get 'tags data)))
    (case (common:match_tags stags etags)
      ('false 'false)
      ('true (funcall next)))))

;;
;; Sends a query request for a match
;;
(defun query_match (shift data)
 (let ((shid (maps:get #"id" shift))
       (uid (maps:get 'uid data)))
  (engine_job_shift:query_match shid uid)
  (kit:mladd 'unmatched_shifts shift data)))

;;
;; This function is called by a shift process
;; on passive employees during a swap
;;
(defun query_swap (sid uid shift o pickup)
  (send sid uid (tuple 'query_swap shift o pickup)))

;;
;; This function is called by a shift process
;; on the initiator employee of a swap
;;
(defun setup_swap (sid uid peers)
  (send sid uid (tuple 'setup_swap peers)))


;;
;; This function is called on peer employee process
;; when no swap handshake that would increase quality is 
;; found. The reply is sent directly to the originator process
;;
(defun reply_swap_no_match (sid ouid uid data)
  (send sid ouid (tuple 'reply_swap 'nomatch uid))
  (tuple 'next_state 'waiting_swap data))

;; 
;; This function is called on peer employee process
;; when a swap handshake that increases quality is found. 
;; The reply is sent directly to the originator process
;;
(defun reply_swap (sid ouid oshift uid shift dh ds dv dh2 ds2 dv2 data)
  (send sid ouid (tuple 'reply_swap oshift uid shift dh ds dv dh2 ds2 dv2))
  (tuple 'next_state 'waiting_swap 
    (maps:put 'swap (tuple ouid oshift uid shift) data )))

;;
;; This function is called on peer employee process
;; in the case of swap handshake that has no originator shift
;; counterpart available
;;
(defun reply_swap (sid ouid uid shift dh ds dv data)
  (send sid ouid (tuple 'reply_swap uid shift dh ds dv))
  (tuple 'next_state 'waiting_swap
    (maps:put 'swap (tuple ouid 'undefined uid shift) data)))

;;
;; This function is called by shift processes on employee processes
;; when comitting a swap
;;
(defun confirm_swap (sid uid op shift)
  (send sid uid (tuple 'confirm_swap op shift)))

;; 
;; Adds the shift and returns the new quality differential
;;
(defun add_shift (data shift next)
  (let* ((data2 (kit:mladd 'matched_shifts shift data))
         (data3 (kit:mlrem 'unmatched_shifts shift data2))
         ((tuple dh ds dv data4)(update_stats data3)))
    (funcall next dh ds dv data4)))


;;
;; Removes the shift and returns the quality differential
;;
(defun remove_shift (data shift next)
  (let* ((data2 (kit:mlrem 'matched_shifts shift data))
         (data3 (kit:mladd 'unmatched_shifts shift data2))
         ((tuple dh ds dv data4)(update_stats data3)))
    (funcall next dh ds dv data4)))

;; Builds a neighbour list of shifts, by either:
;; (a) replacing the (e)xisting shift by the (r)eplacement shift 
;; (b) adding a shift 
;; (c) removing a shift
;;
(defun create_neighbour 
  ((r 'undefined shifts) 
    (tuple r 'undefined (cons r shifts)))
  (('undefined e shifts) 
    (let ((eid (maps:get #"id" e)))
      (tuple 'undefined e (lists:filter 
        (lambda (s) (=/= eid (maps:get #"id" s))) shifts))))
  ((r e shifts)
    (let* ((eid (maps:get #"id" e))
          (n (lists:map (lambda (s)
              (case (== eid (maps:get #"id" s))
                ('true r)
                ('false s))) shifts)))
      (tuple r e n))))

;;
;; Computes the number of hard of soft 
;; rules verified, for the given neighbour and 
;; returns them appended to the given tuple
;;
(defun neighbour_quality 
  (((tuple r e shifts) data)
    (eval_rules shifts data (lambda (h s v _ _ _)
      (tuple r e shifts h s v))))
  ((shifts data)
    (eval_rules shifts data (lambda (h s v _ _ _)
      (tuple shifts h s v)))))

;;
;; Returns the neighbour that has the highest
;; quality, giving priority to hard constraints
;;
(defun max_neighbour_quality (neighbours)
  (max_neighbour_quality neighbours 'undefined))

;;
;; Computes whether q1 (h1, s1, v1) has greater 
;; quality than q2 (h2, s2, v2) 
;;
(defun greater_q? (h1 s1 v1 h2 s2 v2)
  (let ((dh (- h1 h2))
        (ds (- s1 s2))
        (dv (- v1 v2)))
    (q_increase? dh ds dv)))

(defun q_increase? (dh ds dv)
  (or (> dh 0) (and (== dh 0) (or (> ds 0) (and (== ds 0) (> 0 dv))))))

(defun max_neighbour_quality
  (('() r) r)
  (((cons n rest) 'undefined) (max_neighbour_quality rest n))
  (((cons n rest) r) 
    (let (((tuple _ _ _ h1 s1 v1) n)
          ((tuple _ _ _ h2 s2 v2) r))
      (case (greater_q? h1 s1 v1 h2 s2 v2)
        ('true (max_neighbour_quality rest n))
        ('false (max_neighbour_quality rest r))))))


(defun print_swap
  (((tuple oid s uid))
    (let ((o (users:resolve oid))
          (p (users:resolve uid)))
      (io:format "swap: ~p <-- (~p ~p ~p:~p/~p:~p) -- ~p~n"
        (list (maps:get #"email" o)
              (maps:get #"name" s)
              (maps:get #"start_day" s)
              (maps:get #"start_hour" s)
              (maps:get #"start_min" s)
              (maps:get #"end_hour" s)
              (maps:get #"end_min" s)
              (maps:get #"email" p))))))


;;
;; Compares the quality of the given handshake, against the 
;; previous handshake, and retains the one that leads 
;; to the highest global increase of quality
;; 
;;
(defun retain_or_discard_swap_handshake (hsk1 o_q p_dq data next)
  (let ((sid (maps:get 'sid data)))
    (case (maps:get 'swap_handshake data)
      ('undefined 
        (let* (((tuple pdh pds pdv) p_dq)
               ((tuple oh os ov) o_q)
               ((tuple odh ods odv)(quality_delta oh os ov data))
               ((tuple dh ds dv)(tuple (+ odh pdh) (+ ods pds) (+ odv pdv))))
          (case (q_increase? dh ds dv)
            ('true 
              (engine_notifier:emit 'swap_retain sid (tuple hsk1 (tuple odh ods odv) (tuple pdh pds pdv)))
              (funcall next (maps:merge data (map 'swap_handshake hsk1 'swap_handshake_q (tuple dh ds dv)))))
            ('false 
              (engine_notifier:emit 'swap_discard sid (tuple hsk1 (tuple odh ods odv) (tuple pdh pds pdv)))
              (funcall next data)))))
      ((= _ hsk0)
        (let* (((tuple dh0 ds0 dv0) (maps:get 'swap_handshake_q data))
               ((tuple oh os ov) o_q)
               ((tuple odh ods odv)(quality_delta oh os ov data))
               ((tuple pdh pds pdv) p_dq)
               ((tuple dh1 ds1 dv1)(tuple (+ odh pdh) (+ ods pds) (+ odv pdv))))
          (case (greater_q? dh1 ds1 dv1 dh0 ds0 dv0)
            ('true
              (engine_notifier:emit 'swap_discard sid (tuple hsk0 (tuple odh ods odv) (tuple pdh pds pdv)))
              (engine_notifier:emit 'swap_retain sid (tuple hsk1 (tuple odh ods odv) (tuple pdh pds pdv)))
              (funcall next (maps:merge data (map 'swap_handshake hsk1 'swap_handshake_q (tuple dh1 ds1 dv1)))))
            ('false
              (engine_notifier:emit 'swap_discard sid (tuple hsk1 (tuple odh ods odv) (tuple pdh pds pdv)))
              (funcall next data))))))))

;;
;; Confirms the current swap, if any or interrupts the 
;; swap phase
;;
(defun confirm_swap_handshake (data)
  (let ((sid (maps:get 'sid data))
        (uid (maps:get 'uid data))
        (matched_shifts (maps:get 'matched_shifts data)))
    (case (maps:get 'swap_handshake data) 
      ('undefined 
        (engine_job:no_swap_found sid)
        (tuple 'next_state 'waiting_swap data))
      (hsk 
        (engine_job:commit_swap sid hsk)
        (tuple 'next_state 'waiting_swap data)))))


;;
;; Starts a new 'swap query' handshake, by asking other employee processes
;; for a shift
;;
(defun setup_swap_query (data)
  (let ((uid (maps:get 'uid data))
        (unmatched_shifts (maps:get 'unmatched_shifts data))
        (matched_shifts (maps:get 'matched_shifts data)))
    (lists:map (lambda (shift) (engine_job_shift:query_swap (maps:get #"id" shift) uid matched_shifts)) unmatched_shifts)    
    (tuple 'next_state 'setting_up_swap_query (maps:merge data (map 'swap_setup_replies 0 'swap_handshake 'undefined)))))


(defun on_full_swap (data oshift uid pshift dh1 ds1 dv1 dh2 ds2 dv2 next)
  (let* ((sid (maps:get 'sid data))
         (ouid (maps:get 'uid data))
         (data2 (kit:new_counters data (map 'swap_replies 1)))
         (matched_shifts (maps:get 'matched_shifts data2))
         ((tuple _ _ n1)(create_neighbour pshift 'undefined matched_shifts))
         ((tuple _ h1 s1 v1)(neighbour_quality n1 data2))
         ((tuple _ _ n2)(create_neighbour pshift oshift matched_shifts))
         ((tuple _ h2 s2 v2)(neighbour_quality n2 data2)))
    (engine_notifier:emit 'swap_reply_full sid uid)
    (retain_or_discard_swap_handshake (tuple ouid pshift uid) (tuple h1 s1 v1) (tuple dh1 ds1 dv1) data2 (lambda (data3)
      (retain_or_discard_swap_handshake (tuple oshift ouid pshift uid) (tuple h2 s2 v2) (tuple dh2 ds2 dv2) data3 next)))))

(defun on_partial_swap (data uid pshift dh1 ds1 dv1 next)
  (let* ((sid (maps:get 'sid data))
         (ouid (maps:get 'uid data))
         (data2 (kit:new_counters data (map 'swap_replies 1)))
         (matched_shifts (maps:get 'matched_shifts data2))
         ((tuple _ _ n1)(create_neighbour pshift 'undefined matched_shifts))
         ((tuple _ h1 s1 v1)(neighbour_quality n1 data2)))
    (engine_notifier:emit 'swap_reply_partial sid uid)
    (retain_or_discard_swap_handshake (tuple ouid pshift uid) (tuple h1 s1 v1) (tuple dh1 ds1 dv1) data2 next)))


  

;;
;; 
;; gen_fsm module callback 
;;
(defun init (((list sid o u))
  (register sid u)
  (tuple 'ok 'starting (map 'sid sid 'u u 'o o ) 0)))

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
          (o (maps:get 'o data)))
        (organizations:get_member_tags o u (lambda (tags)
          (schedules:with_organization_member_rules u o (lambda (rules)
            (schedules:with_schedule sid (lambda (s) 
              (let* ((hc (lists:filter (lambda (r) (== #"true" (maps:get #"strong" r))) rules))
                     (sc (lists:filter (lambda (r) (== #"false" (maps:get #"strong" r))) rules))
                     (y (kit:bin2int (maps:get #"year" s)))
                     (w (kit:bin2int (maps:get #"week" s))))
                (schedules:with_availability u o y w (lambda (a)
                  (stats:with_weekly_stats u #"scheduled" y w (lambda (stats)
                    (set_availability_shifts u a stats (lambda (avshifts stats2)
                      (engine_job:employee_waiting sid uid)
                        (tuple 'next_state 'waiting (maps:merge
                          (map 's s 'uid uid 'tags tags 
                            'rules rules 
                            'h_rules_verified 0 
                            's_rules_verified 0 
                            'rule_violations 0 
                            'rules_verified '() 
                            'rules_non_verified '()
                            'h_rules_total (length hc) 's_rules_total (length sc)  
                            'unmatched_shifts '() 'matched_shifts '() 
                            'stats_before stats2 'stats stats2 'avshifts avshifts) data))))))))))))))))))

(defun waiting

  ;;
  ;; We get a request from the job process 
  ;; to attempt to match the given shift
  ;;
  (((tuple 'match shift) data)
    (case (match? shift data)
      ('false 
        (engine_job_shift:pass_match (maps:get #"id" shift))
        (tuple 'next_state 'waiting data)) 
      ('true 
        (let* ((sid (maps:get 'sid data))
              (uid (maps:get 'uid data))
              (h (maps:get 'h_rules_total data))
              (s (maps:get 's_rules_total data))
              (data2 (query_match shift data)))
          (engine_job:employee_matching sid uid h s)
          (tuple 'next_state 'matching data2)))))

  ;;
  ;; We get a request from the job process
  ;; to assign the shift
  (((tuple 'assign shift) data)
    (let ((sid (maps:get 'sid data))
          (uid (maps:get 'uid data))
          (h (maps:get 'h_rules_total data))
          (s (maps:get 's_rules_total data)))
      (add_shift data shift (lambda (dh ds _ data2)
        (engine_job:shift_assigned sid shift dh h ds s)
          (tuple 'next_state 'assigning data2))))))

(defun assigning
  
  ;;
  ;; We get a request from the job process
  ;; to assign the shift
  (((tuple 'assign shift) data)
    (let ((sid (maps:get 'sid data))
          (uid (maps:get 'uid data)))
      (add_shift data shift (lambda (dh ds _ data2)
        (engine_job:shift_assigned sid shift dh ds)
        (tuple 'next_state 'assigning data2))))))

(defun matching 
  
  ;;
  ;; We get a request from the job process
  ;; to attempt to match the given shift 
  ;;
  (((tuple 'match shift) data)
    (case (match? shift data)
      ('false (tuple 'next_state 'matching data))
      ('true (tuple 'next_state 'matching (query_match shift data)))))
  
  ;;
  ;; A match request was granted by a shift process
  ;;
  (((tuple 'grant_match shift) data)
    (let* ((uid (maps:get 'uid data))
           (sid (maps:get 'sid data))
           (shid (maps:get #"id" shift)))
      (add_shift data shift (lambda (dh ds _ data2)
        (engine_job:quality sid dh ds) 
        (engine_job_shift:ack_match shid uid)
        (tuple 'next_state 'matching data2)))))

  ;;
  ;; The matching phase is finished we are
  ;; ready to swap
  ;;
  (('matched data)
    (tuple 'next_state 'waiting_swap data)))

(defun waiting_swap

  (('swap data)
    (let ((sid (maps:get 'sid data))
          (uid (maps:get 'uid data))
          (h (maps:get 'h_rules_verified data))
          (s (maps:get 's_rules_verified data))
          (unmatched_shifts (maps:get 'unmatched_shifts data)))
      (case (length unmatched_shifts)
        (0
          (engine_job:swap sid uid)
          (tuple 'next_state 'starting_swap data))
        (_
          (engine_job:swap sid uid h s)
          (tuple 'next_state 'starting_swap data)))))

  ;;
  ;; A shift process forwards a query for a swap
  ;; from the initiator employee to each peer employee. Peer
  ;; employees reply with either a handshake or a nomatch,
  ;; directly to the originator process
  ;;
  (((tuple 'query_swap shift ouid oshifts) data)
    (let* ((sid (maps:get 'sid data))
           (shid (maps:get #"id" shift))
           (uid (maps:get 'uid data))
           (h (maps:get 'h_rules_verified data))
           (s (maps:get 's_rules_verified data))
           (matched_shifts (maps:get 'matched_shifts data))
           ((tuple 'undefined _ n)(create_neighbour 'undefined shift matched_shifts))
           ((tuple _ h s v)(neighbour_quality n data))
           ((tuple dh ds dv)(quality_delta h s v data)))
      (case (length oshifts)
        (0 (reply_swap sid ouid uid shift dh ds dv data))
        (_
          (let* ((m_oshifts (match_shifts oshifts data))
                 (n (lists:map (lambda (oshift) (create_neighbour oshift shift matched_shifts)) m_oshifts))
                 (n_q (lists:map (lambda (n) (neighbour_quality n data)) n))
                 (max_n (max_neighbour_quality n_q)))
            (case max_n 
              ('undefined (reply_swap sid ouid uid shift dh ds dv data))
              ((tuple oshift _ _ h2 s2 v2) 
                (let (((tuple dh2 ds2 dv2)(quality_delta h2 s2 v2 data)))
                  (reply_swap sid ouid oshift uid shift dh ds dv dh2 ds2 dv2 data)))))))))
                
  ;;
  ;; The shift is transfered to the peer employee process
  ;;
  (((tuple 'confirm_swap 'add shift) data)
    (let ((uid (maps:get 'uid data))
          (shid (maps:get #"id" shift)))
      (add_shift data shift (lambda (dh ds _ data2)
        (engine_job_shift:ack_swap shid uid dh ds)
        (tuple 'next_state 'waiting_swap data2)))))

  ;;
  ;; The shift is transfered from the peer employee process
  ;;
  (((tuple 'confirm_swap 'remove shift) data)
    (let ((uid (maps:get 'uid data))
          (shid (maps:get #"id" shift)))
      (remove_shift data shift (lambda (dh ds _ data2)
        (engine_job_shift:ack_swap shid uid dh ds)
        (tuple 'next_state 'waiting_swap data2)))))


  ;;
  ;; Here we dont need to do anything special, just
  ;; ack back and stay in the same state
  ;;
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'waiting_swap data))))
  

(defun starting_swap
  
  ;;
  ;; The swap application as the initiator was granted
  ;; The employee participates in this swap phase in
  ;; active mode
  ;;
  (('grant_swap data)
    (let ((sid (maps:get 'sid data))
          (uid (maps:get 'uid data))
          (unmatched_shifts (maps:get 'unmatched_shifts data))
          (matched_shifts (maps:get 'matched_shifts data)))
      (case (length unmatched_shifts)
        (0
          (engine_job:abort_swap sid)
          (tuple 'next_state 'waiting_swap data))
        (_
          (engine_job:ack_grant_swap sid uid)
          (setup_swap_query data)))))
  
  ;;
  ;; The swap application as the initiator was denied 
  ;; The employee still can participate in the swap phase
  ;; in passive mode
  ;;
  (('deny_swap data)
    (let ((sid (maps:get 'sid data))
          (uid (maps:get 'uid data)))
      (engine_job:ack_deny_swap sid uid)
      (tuple 'next_state 'waiting_swap data)))
  
  ;;
  ;; Interrupt the process - ack back and get ready
  ;; for the next phase
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'waiting_swap data))))


(defun setting_up_swap_query
  
  ;;
  ;; A shift process replies to the initial swap query
  ;; by providing the potential number of passive employee peers
  ;;
  (((tuple 'setup_swap peers) data)
    (let* ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'swap_setup_replies 1 'swap_peers peers)))
          (replies (maps:get 'swap_setup_replies data2))
          (unmatched_shifts (maps:get 'unmatched_shifts data2)))
      (case (== replies (length unmatched_shifts))
        ('false (tuple 'next_state 'setting_up_swap_query data2))
        ('true (tuple 'next_state 'swapping data2)))))
  
  ;; A reply from a peer employee process that has
  ;; no shift to offer
  ;;
  (((tuple 'reply_swap 'nomatch uid) data)
    (let ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'swap_replies 1))))
      (engine_notifier:emit 'swap_reply_nomatch sid uid)
      (tuple 'next_state 'setting_up_swap_query data)))

  ;;
  ;; A reply from a peer employee that has a shift
  ;; to offer in exchange for the originator's shift
  ;;
  (((tuple 'reply_swap oshift uid pshift dh1 ds1 dv1 dh2 ds2 dv2) data)
    (on_full_swap data oshift uid pshift dh1 ds1 dv1 dh2 ds2 dv2 (lambda (data2)
      (let ((swap_replies (maps:get 'swap_replies data2))
            (swap_peers (maps:get 'swap_peers data2)))
        (case (== swap_replies swap_peers)
          ('false (tuple 'next_state 'setting_up_swap_query data2))
          ('true (confirm_swap_handshake data2)))))))
  
  ;;
  ;; A reply from a peer employee that has a shift
  ;; to offer, for nothing in exchange
  ;;
  (((tuple 'reply_swap uid pshift dh1 ds1 dv1) data)
    (on_partial_swap data uid pshift dh1 ds1 dv1(lambda (data2)
      (let ((swap_replies (maps:get 'swap_replies data2))
            (swap_peers (maps:get 'swap_peers data2)))
        (case (== swap_replies swap_peers)
          ('false (tuple 'next_state 'setting_up_swap_query data2))
          ('true (confirm_swap_handshake data2)))))))
  ;;
  ;; Interrupt the process - ack back and get ready
  ;; for the next phase
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'waiting_swap data))))



(defun swapping

  ;; 
  ;; A reply from a peer employee process that has
  ;; no shift to offer
  ;;
  (((tuple 'reply_swap 'nomatch uid) data)
    (let* ((sid (maps:get 'sid data))
          (data2 (kit:new_counters data (map 'swap_replies 1)))
          (swap_replies (maps:get 'swap_replies data2))
          (swap_peers (maps:get 'swap_peers data2)))
      (engine_notifier:emit 'swap_reply_nomatch sid uid)
      (case (== swap_replies swap_peers)
        ('false (tuple 'next_state 'swapping data2))
        ('true (confirm_swap_handshake data2)))))
  
  ;;
  ;; A reply from a peer employee that has a shift
  ;; to offer in exchange for the originator's shift
  ;;
  (((tuple 'reply_swap oshift uid pshift dh1 ds1 dv1 dh2 ds2 dv2) data)
    (on_full_swap data oshift uid pshift dh1 ds1 dv1 dh2 ds2 dv2 (lambda (data2)
      (let ((swap_replies (maps:get 'swap_replies data2))
            (swap_peers (maps:get 'swap_peers data2)))
        (case (== swap_replies swap_peers)
          ('false (tuple 'next_state 'swapping data2))
          ('true (confirm_swap_handshake data2)))))))

  ;;
  ;; A reply from a peer employee that has a shift
  ;; to offer, for nothing in exchange
  ;;
  (((tuple 'reply_swap uid pshift dh1 ds1 dv1) data)
    (on_partial_swap data uid pshift dh1 ds1 dv1 (lambda (data2)
      (let ((swap_replies (maps:get 'swap_replies data2))
            (swap_peers (maps:get 'swap_peers data2)))
        (case (== swap_replies swap_peers)
          ('false (tuple 'next_state 'swapping data2))
          ('true (confirm_swap_handshake data2)))))))
    
  ;;
  ;; Interrupt the process - ack back and get ready
  ;; for the next phase
  (('finish_swap data)
    (let ((sid (maps:get 'sid data)))
      (engine_job:ack_finish_swap sid)
      (tuple 'next_state 'waiting_swap data))))

;_ ;
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
          (s (maps:get 's data))
          (u (maps:get 'u data))
          (uid (maps:get 'uid data))
          (y (kit:bin2int (maps:get #"year" s)))
          (w (kit:bin2int (maps:get #"week" s)))
          (stats (maps:get 'stats data))
          (rules_verified (maps:get 'rules_verified data))
          (rules_non_verified (maps:get 'rules_non_verified data)))
      (schedules:add_verified_constraints s rules_verified uid (lambda()
        (schedules:add_nonverified_constraints s rules_non_verified uid (lambda ()
          (schedules:add_participant_stats s u stats (lambda ()
            (engine_job:employee_stopped sid uid)
            (tuple 'next_state 'end data))))))))))

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
