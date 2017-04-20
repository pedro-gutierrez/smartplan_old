(defmodule schedules
  (export all))

(defun create_template (u o name next) 
  (common:on_new_name o #"templates" name (lambda()
    (let ((owner (maps:get #"id" u))
          (id (kit:uuid)))
      (db:hmset (list #"template" id)
        (map #"id" id #"organization" (maps:get #"id" o) #"name" name #"createdby" owner #"created" (kit:now) #"ownedby" owner #"status" #"active") (lambda (t)
          (common:add_name o #"templates" name (lambda()
            (db:add_child o t #"templates" (lambda () 
              (funcall next t)))))))))))

(defun create_availability_template (u o name next) 
  (common:on_new_name o #"avtemplates" name (lambda()
    (let ((owner (maps:get #"id" u))
          (id (kit:uuid)))
      (db:hmset (list #"avtemplate" id)
        (map #"id" id #"organization" (maps:get #"id" o) #"name" name #"createdby" owner #"created" (kit:now) #"ownedby" owner #"status" #"active") (lambda (t)
          (common:add_name o #"avtemplates" name (lambda()
            (db:add_child o t #"avtemplates" (lambda () 
              (funcall next t)))))))))))


(defun with_templates (parent next) 
  (db:join_alpha
    (list #"templates" (maps:get #"id" parent))
    #"template"
    (list #"name" #"organization" #"id" #"status" #"created" #"ownedby" #"createdby")
    #"name"
    next))

(defun with_availability_templates (parent next) 
  (db:join_alpha
    (list #"avtemplates" (maps:get #"id" parent))
    #"avtemplate"
    (list #"name" #"organization" #"id" #"status" #"created" #"ownedby" #"createdby")
    #"name"
    next))

(defun with_templates (u p next)
  (with_templates p (lambda (templates) 
    (lists:map (lambda (t) 
      (common:with_access u t (lambda (t2) t2))) templates ))))

(defun with_availability_templates (u p next)
  (with_availability_templates p (lambda (templates) 
    (lists:map (lambda (t) 
      (common:with_access u t (lambda (t2) t2))) templates ))))

(defun with_template (u id next)
  (with_template id (lambda (t)
    (common:with_access u t (lambda (t2)
      (funcall next t2))))))

(defun with_template (id next ) 
  (db:hgetall (list #"template" id) next )) 

(defun resolve_template (id)
  (with_template id (lambda (t) t)))

(defun with_availability_template (u id next)
  (with_availability_template id (lambda (t)
    (common:with_access u t (lambda (t2)
      (funcall next t2))))))

(defun with_availability_template (id next ) 
  (db:hgetall (list #"avtemplate" id) next )) 

(defun rename_template (u s t name next) 
  (let ((old_name (maps:get #"name" t)))
    (case (== old_name name )
      ('true (funcall next t))
      ('false 
        (common:on_new_name s #"templates" name (lambda()
          (db:hmset (list #"template" (maps:get #"id" t)) (map #"name" name) (lambda (t2) 
            (common:add_name s #"templates" name (lambda() 
              (common:remove_name s #"templates" old_name (lambda () 
                (common:with_access u t2 (lambda (t3) t3))))))))))))))

(defun set_availability_template_owner (u t o next)
  (db:hmset (list #"avtemplate" (maps:get #"id" t)) (map #"ownedby" (maps:get #"id" o)) (lambda (t2)
    (common:with_access u t2 (lambda(t3)
      (funcall next t3))))))

(defun rename_availability_template (u s t name next) 
  (let ((old_name (maps:get #"name" t)))
    (case (== old_name name )
      ('true (funcall next t))
      ('false 
        (common:on_new_name s #"avtemplates" name (lambda()
          (db:hmset (list #"avtemplate" (maps:get #"id" t)) (map #"name" name) (lambda (t2) 
            (common:add_name s #"avtemplates" name (lambda() 
              (common:remove_name s #"avtemplates" old_name (lambda () 
                (common:with_access u t2 (lambda (t3) t3))))))))))))))

(defun set_template_owner (u t o next)
  (db:hmset (list #"template" (maps:get #"id" t)) (map #"ownedby" (maps:get #"id" o)) (lambda (t2)
    (common:with_access u t2 (lambda(t3)
      (funcall next t3))))))

(defun add_template_tag (t tag next) 
  (common:add_tag t tag next)) 

(defun remove_template_tag (t tag next)
  (common:remove_tag t tag next))

(defun with_template_tags (t next)
  (common:get_tags t next ))

(defun with_shift_tags (s next)
  (common:get_tags s next))
;;
;; Returns the duration of a shift given
;; its start and end times, in number of minutes
;;
(defun shift_duration 
  ((sd sh 0 ed eh 0) (* 60 (- eh sh)))
  ((sd sh sm ed eh 0) (+ (- 60 sm) (* 60 (- eh sh 1)))) 
  ((sd sh 0 ed eh em) (+ em (* 60 (- eh sh)) )) 
  ((sd sh sm ed eh em) (+ ( - 60 sm ) em (* 60 ( - eh sh 1) ))))

(defun with_shift_type ( t next )
  (case (sets:is_element t (sets:from_list (kit:cfg 'schedules 'shift_types )))
    ('true (funcall next t))
    ('false (kit:err 'not_found t))))

(defun add_template_shift (u t name type staffing sd sh sm ed eh em next ) 
  (let ((id (kit:uuid))
        (owner (maps:get #"id" u))
        (tid (maps:get #"id" t))
        (oid (maps:get #"organization" t))
        (duration (shift_duration sd sh sm ed eh em )))
    (with_shift_type type (lambda (stype)
      (db:hmset (list #"shift" id)
        (map #"id" id #"template" tid #"organization" oid #"name" name #"type" stype  #"staffing" staffing #"start_day" sd #"start_hour" sh #"start_min" sm 
          #"end_day" ed #"end_hour" eh #"end_min" em #"created" (kit:now) #"createdby" owner #"ownedby" owner #"duration" duration ) (lambda(s)
            (db:add_child t s #"shifts" (lambda () 
              (common:with_access u s next)))))))))

(defun update_template_shift (s name type staffing sd sh sm ed eh em next) 
  (let ((duration (shift_duration sd sh sm ed eh em)))
    (with_shift_type type (lambda (stype)
      (db:hmset (list #"shift" (maps:get #"id" s))
        (map #"name" name #"type" stype #"staffing" staffing #"start_day" sd #"start_hour" sh #"start_min" sm #"end_day" 
           ed #"end_hour" eh #"end_min" em #"duration" duration ) (lambda(s2)
          (funcall next s2)))))))


(defun copy_template_shift (u t s name type staffing sd sh sm ed eh em next) 
  (add_template_shift u t name type staffing sd sh sm ed eh em (lambda (s2)
    (common:with_tags s (lambda (tags)
      (common:add_tags s2 tags (lambda (_)
        (funcall next s2))))))))

(defun add_availability_template_shift (u t shift next)
  (add_availability_template_shift u t 
    (kit:bin2int (maps:get #"start_day" shift))
    (kit:bin2int (maps:get #"start_hour" shift))
    (kit:bin2int (maps:get #"start_min" shift))
    (kit:bin2int (maps:get #"end_day" shift))
    (kit:bin2int (maps:get #"end_hour" shift))
    (kit:bin2int (maps:get #"end_min" shift)) next))


(defun add_availability_template_shift (u t shift)
  (add_availability_template_shift u t shift (lambda (s) s)))

(defun add_availability_template_shift (u t sd sh sm ed eh em next ) 
  (let* ((id (kit:uuid))
         (tid (maps:get #"id" t))
         (oid (maps:get #"organization" t))
         (owner (maps:get #"id" u))
         (duration (kit:int2bin (shift_duration sd sh sm ed eh em )))
         (s (map #"id" id #"template" tid  #"organization" oid #"type" #"available" #"start_day" (kit:int2bin sd) #"start_hour" (kit:int2bin sh) #"start_min" (kit:int2bin sm)
                 #"end_day" (kit:int2bin ed) #"end_hour" (kit:int2bin eh) #"end_min" (kit:int2bin em) #"created" (kit:now) #"createdby" owner #"ownedby" owner #"duration" duration )))
    (with_template_shifts t (lambda (shifts)
      (no_overlap? (sort_template_shifts (cons s shifts)) (lambda ()
        (db:hmset (list #"shift" id) s (lambda (s2)
          (db:add_child t s2 #"shifts" (lambda () 
            (common:with_access u s2 next)))))))))))

(defun update_availability_template_shift (s sd sh sm ed eh em next) 
  (let* ((id (maps:get #"id" s))
         (tid (maps:get #"template" s)))
    (with_availability_template tid (lambda (t)
      (let* ((duration (kit:int2bin (shift_duration sd sh sm ed eh em)))
             (s2 (maps:merge s (map #"start_day" (kit:int2bin sd) #"start_hour" (kit:int2bin sh) #"start_min" (kit:int2bin sm) 
                                    #"end_day" (kit:int2bin ed) #"end_hour" (kit:int2bin eh) #"end_min" (kit:int2bin em) #"duration" duration))))
        (with_template_shifts t (lambda (shifts)
          (no_overlap? (sort_template_shifts (replace_shift s2 shifts)) (lambda ()
            (db:hmset (list #"shift" id) s2 next))))))))))

(defun copy_availability_template_shift (u t s sd sh sm ed eh em next) 
  (add_availability_template_shift u t sd sh sm ed eh em next))

(defun remove_template_shift (u t s next) 
  (remove_template_shift t s next))

(defun remove_template_shift (t s next) 
  (db:del (list #"shift" (maps:get #"id" s)) (lambda ()
    (db:remove_child t s #"shifts" next ))))

(defun with_template_shifts (t next) 
  (db:join
    (list #"shifts" (maps:get #"id" t))
    #"shift"
    (list #"id" #"template" #"organization" #"name" #"staffing" #"start_day" #"start_hour" #"start_min" #"end_day" #"end_hour" #"end_min" #"duration" #"type" )
    #"start_day" 
    next))

(defun with_template_shifts (u t next)
  (with_template_shifts t next))

(defun with_template_shift (u sid next)
  (db:hgetall (list #"shift" sid) (lambda (s) 
    (common:with_access u s (lambda (s2) 
      (common:with_tags s2 (lambda (tags)
        (funcall next (maps:put #"tags" tags s2)))))))))

(defun add_shift_tag (s tag next) 
  (common:add_tag s tag next ))

(defun remove_shift_tag (s tag next)
  (common:remove_tag s tag next ))

(defun create_profile (u o name next)
  (common:on_new_name o #"profiles" name (lambda()
    (let ((owner (maps:get #"id" u))
          (id (kit:uuid)))
      (db:hmset (list #"profile" id) 
        (map #"id" id #"organization" (maps:get #"id" o) #"name" name #"tag" (kit:bin2lower name) 
             #"createdby" owner #"created" (kit:now) #"ownedby" owner #"status" #"active") (lambda (p)
          (common:add_name o #"profiles" name (lambda()
            (db:add_child o p #"profiles" (lambda () 
              (common:with_access u p (lambda (p2) p2))))))))))))

(defun rename_profile (u o p name next) 
  (let ((old_name (maps:get #"name" p)))
    (case (== old_name name )
      ('true (funcall next p))
      ('false
          (common:on_new_name o #"profiles" name (lambda()
            (db:hmset (list #"profile" (maps:get #"id" p)) (map #"name" name) (lambda (p2) 
              (common:add_name o #"profiles" name (lambda()
                (common:remove_name o #"profiles" old_name (lambda ()
                  (common:with_access u p2 next)))))))))))))


(defun set_profile_owner (u p o next)
  (db:hmset (list #"profile" (maps:get #"id" p)) (map #"ownedby" (maps:get #"id" o)) (lambda (p2)
    (common:with_access u p2 (lambda(p3)
      (funcall next p3))))))

;;
;; Get a profile, by id - with callback
;; version
;;
(defun with_profile (u id next)
  (with_profile id (lambda (p)
    (common:with_access u p (lambda (p2)
      (funcall next p2))))))

(defun with_profile (id next)
  (db:hgetall (list #"profile" id) next ))

;;
;; Get a profile, by id - return the 
;; profile 
;;
(defun with_profile (id) 
  (with_profile id (lambda (p) p )))

;;
;; Get all the profiles for the given organization
;;
(defun with_profiles (u o next)
  (with_profiles o (lambda (profiles) 
    (funcall next (lists:map (lambda (p) 
      (common:with_access u p (lambda (p2) p2 ))) profiles)))))

(defun with_profiles (o next)
  (db:join_alpha
    (list #"profiles" (maps:get #"id" o))
    #"profile"
    (list #"name" #"tag" #"organization" #"id" #"status" #"created" #"ownedby" #"createdby")
    #"name"
    next))

;; 
;; Returns the list of profiles for 
;; the specified user in the specified organization
;; - we look at each tag and find out whether
;; or not it is a profile
;;
(defun with_user_profiles (u o next) 
  (with_profiles o (lambda (profiles)
    (common:get_tags u o (lambda (t) 
      (let ((tags (sets:from_list t)))
        (funcall next (lists:filter (lambda (p) (sets:is_element (maps:get #"tag" p) tags)) profiles)))))))) 

(defun with_indicators (next)
  (funcall next (indicators))) 

(defun indicators ()
  (lists:map (lambda (props) (maps:from_list props))(kit:cfg 'schedules 'indicators)))

;;
;; Gets an indicator, given its id
;;
(defun with_indicator (u id next)
  (with_indicator id next))

;;
;; Gets an indicator, given its id
;;
(defun with_indicator (id next)
  (let* ((indicators (indicators))
         (ind (lists:filter (lambda (i) (== id (maps:get 'id i))) indicators)))
    (case ind
      ('() (kit:err 'not_found 'indicator))
      ((cons i _) (funcall next i)))))

(defun create_profile_rule (u p i strong op value next)
  (let ((id (kit:uuid))
        (owner (maps:get #"id" u)))
    (db:hmset (list #"rule" id)
      (map #"id" id #"profile" (maps:get #"id" p) #"indicator" (maps:get 'id i) #"strong" strong #"op" op #"value" (kit:num2bin value)  
        #"created" (kit:now) #"createdby" owner #"ownedby" owner #"status" #"active" ) (lambda(r)
          (db:add_child p r #"rules" (lambda () 
            (common:with_access u r next)))))))

(defun update_profile_rule (u p r strong op value next)
  (db:hmset (list #"rule" (maps:get #"id" r)) (map #"strong" strong #"op" op #"value" (kit:num2bin value)) (lambda (r2)
    (common:with_access u r2 (lambda(r3)
      (funcall next r3)))))) 

(defun disable_profile_rule (u p r next)
  (db:hmset (list #"rule" (maps:get #"id" r)) (map #"status" #"inactive" ) (lambda (r2)
    (common:with_access u r2 (lambda(r3)
      (funcall next r3))))))

(defun enable_profile_rule (u p r next)
  (db:hmset (list #"rule" (maps:get #"id" r)) (map #"status" #"active" ) (lambda (r2)
    (common:with_access u r2 (lambda(r3)
      (funcall next r3))))))

;;
;; Get the rules for the given profile
;; 
(defun with_profile_rules (u p next)
  (common:with_access u p (lambda (p2)
    (with_profile_rules p (lambda (rules)
      (funcall next (lists:map (lambda (r)
        (maps:put 'access (maps:get 'access p2) r)) rules )))))))

;;
;; Get the rules for the given profile
;;
(defun with_profile_rules (p next)
  (db:join_alpha
    (list #"rules" (maps:get #"id" p))
    #"rule"
    (list #"id" #"indicator" #"profile" #"strong" #"op" #"value" #"status")
    #"indicator"
    next))

;;
;; Given a user and an organization, 
;; return the set of rules, for all profiles
;;
(defun with_organization_member_rules (u o next)
  (organizations:with_member o u (lambda (m)
    (case (maps:is_key #"profile" m)
      ('false (funcall next '()))
      ('true
        (with_profile (maps:get #"profile" m) (lambda (p)
          (with_profile_rules u p next))))))))
  
  ;;(with_user_profiles u o (lambda (profiles)
  ;;  (collect_all_profile_rules profiles '() (lambda (rules)
  ;;    (funcall next rules))))))

;;
;; Given the list of profiles, 
;; recursively return all associated rules
;;
(defun collect_all_profile_rules
  ((() all_rules next) (funcall next (lists:flatten all_rules)))
  (((cons p rem) all_rules next)
    (with_profile_rules p (lambda (rules)
      (collect_all_profile_rules rem (cons (filter_rules rules) all_rules) next)))))

(defun with_profile_rule (u p id next) 
  (db:hgetall (list #"rule" id) (lambda (r)
    (common:with_access u r (lambda (r2)
      (funcall next r2))))))

(defun remove_profile_rule (u p r next) 
  (db:del (list #"rule" (maps:get #"id" r)) (lambda ()
    (db:remove_child p r #"rules" next ))))

(defun with_schedules (u o next)
  (with_schedules o (lambda (schedules)
    (funcall next (lists:map (lambda (s) 
      (common:with_access u s (lambda (s2) s2))) schedules)))))

(defun with_schedules (p next)
  (with_schedules_by_key (list #"schedules" (maps:get #"id" p)) next))

(defun with_schedules_by_month (o y m next)
  (with_schedules_by_key (list #"schedules-m" (kit:int2bin y) (kit:int2bin m) (maps:get #"id" o)) next))

(defun with_schedules_by_week (o y w next)
  (with_schedules_by_key (list #"schedules-w" (kit:int2bin y) (kit:int2bin w) (maps:get #"id" o)) next))

(defun with_schedules_by_key (k next)
  (db:join_alpha
    k
    #"schedule"
    (list #"id" #"week" #"year" #"status" #"created" #"ownedby" #"createdby" #"template" #"organization" #"auto" )
    #"rank"
    next))

(defun with_schedule_key (t w y next)
  (funcall next (list #"key-schedule" (maps:get #"id" t) (kit:int2bin y) (kit:int2bin  w))))
  
(defun with_schedule (u o t w y next)
  (with_schedule_key t w y (lambda (k)
    (db:get k (lambda (id)
      (db:hgetall (list #"schedule" id) (lambda (s)
        (common:with_access u s (lambda (s2)
          (funcall next s2))))))))))

(defun with_schedule (u o t w y )
  (with_schedule u o t w y (lambda (s) s )))

(defun with_schedule (u id next)
  (with_schedule id (lambda (s)
    (common:with_access u s (lambda (s2)
      (funcall next s2))))))

(defun with_schedule (id next)
  (db:hgetall (list #"schedule" id) next ))

(defun with_schedule_rank (y w o t next) 
  (let (((list w2) (io_lib:format "~2..0B" (list w))))
    (funcall next (kit:catbin (list (kit:int2bin y) (list_to_binary w2) (maps:get #"id" o) (maps:get #"id" t)))))) 

(defun create_schedule (u o t w y next)
  (with_schedule_key t w y (lambda (k)
    (db:setnx k (lambda (id)
      (with_schedule_rank y w o t (lambda (rank)
      (db:hmset (list #"schedule" id)
        (map #"id" id #"organization" (maps:get #"id" o) #"template" (maps:get #"id" t) #"status" #"draft" #"week" w #"year" y #"rank" rank
          #"created" (kit:now) #"createdby" (maps:get #"id" u) #"ownedby" (maps:get #"id" u) #"auto" #"true") (lambda(s)
          (index_schedule o t s (lambda ()    
            (with_schedule_empty_stats s (lambda (_)
              (common:with_access u s next))))))))))))))

(defun each_template 
  ((o '() finish each) (funcall finish))
  ((o (cons t rest) finish each)
    (funcall each o t (lambda () (each_template o rest finish each)))))
      
(defun each_schedule 
  ((o t '() finish each)(funcall finish))
  ((o t (cons s rest) finish each)
    (funcall each o t s (lambda () (each_schedule o t rest finish each)))))


(defun index_schedules (next)
  (organizations:with_organizations (lambda (orgs)
    (organizations:each orgs next (lambda (o n0)
      (index_schedules o n0))))))  
  
(defun index_schedules (o next)
  (with_templates o (lambda (templates)
    (each_template o templates next (lambda (o t n1)
      (with_schedules t (lambda (schedules)
        (each_schedule o t schedules n1 (lambda (o t s n2)
          (index_schedule o t s n2))))))))))

(defun index_schedule (o t s next)
  (let* ((w (maps:get #"week" s)) 
         (y (maps:get #"year" s))
         (w_int (kit:bin2int w))
         (y_int (kit:bin2int y))
         ((= (tuple my mm md) mday) (kit:goto_monday (kit:goto_week y_int w_int)))
         ((tuple sy sm sd) (kit:goto_sunday mday)))
    (db:add_child o s #"schedules" (lambda () 
      (db:add_child t s #"schedules" (lambda ()
        (db:add_child o s (kit:joinbin (list #"schedules-w" y w ) #"-") (lambda ()
          (case (== mm sm )
            ('true (db:add_child o s (kit:joinbin (list #"schedules-m" y (kit:int2bin mm)) #"-") next))
            ('false
              (db:add_child o s (kit:joinbin (list #"schedules-m" y (kit:int2bin mm)) #"-") (lambda ()
                (db:add_child o s (kit:joinbin (list #"schedules-m" y (kit:int2bin sm)) #"-") next)))))))))))))


  


;; If the shift is published and assigned, then
;; inform the assignee that the shift is no longer assigned
(defun notify_shift_unassigned (s shift next)
  (case (maps:get #"published" shift)
    (#"false" (funcall next))
    (#"true"
      (case (maps:get #"status" shift)
        (#"unassigned" (funcall next))
        (#"assigned" 
          (users:find_by_id (maps:get #"assignedto" shift) (lambda (assignee)
            (notifications:shift assignee #"shift_unassigned" shift next ))))))))

(defun notify_shift_conflict (assignee shift next)
  (case (maps:get #"conflict" shift)
    (#"false" (funcall next))
    (#"true" 
      (case (maps:get #"published" shift)
        (#"false" (funcall next))
        (#"true"
          (case (maps:get #"status" shift)
            (#"unassigned" (funcall next))
            (#"assigned" (notifications:shift assignee #"shift_conflict" shift next ))))))))


;; Maps the shift type to a weekly stat
(defun shift_to_weekly_stat (shift stats next)
  (case (maps:get #"type" shift)
    (#"training" (funcall next #"total_training_time_w" (maps:get #"total_training_time_w" stats)))
    (#"idle" (funcall next #"total_idle_time_w" (maps:get #"total_idle_time_w" stats)))
    (#"std"
      (let ((extra_time (maps:get #"total_extra_time_w" stats)))
        (case (> extra_time 0 )
          ('true (funcall next #"total_extra_time_w" (maps:get #"total_extra_time_w" stats)))
          ('false (funcall next #"total_std_time_w" (maps:get #"total_std_time_w" stats))))))))

(defun shift_type_to_weekly_stat (shift)
  (case (maps:get #"type" shift)
    (#"training" #"total_training_time_w")
    (#"idle" #"total_idle_time_w")
    (#"std" #"total_std_time_w")))

(defun add_shift_duration_to_weekly_stats (shift stats next)
  (let* ((stat (shift_type_to_weekly_stat shift))
        (curr (maps:get stat stats))
        (d (kit:bin2int (maps:get #"duration" shift)))
        (stats2 (maps:put stat (+ d curr) stats))
        (xtime (maps:get #"xtime" shift))
        (xd (kit:bin2int (maps:get #"xduration" shift))))
    (case xtime
      (#"false" (funcall next stats2))
      (#"true"
        (let* ((total_extra_time_w (maps:get #"total_extra_time_w" stats2))
              (total_std_time_w (maps:get #"total_std_time_w" stats2))
              (stats3 (maps:put #"total_extra_time_w" (+ xd total_extra_time_w ) stats2))
              (stats4 (maps:put #"total_std_time_w" (- total_std_time_w xd ) stats3)))
          (funcall next stats4))))))

(defun remove_shift_duration_from_weekly_stats (shift stats next)
  (let* ((stat (shift_type_to_weekly_stat shift))
        (curr (maps:get stat stats))
        (d (kit:bin2int (maps:get #"duration" shift)))
        (stats2 (maps:put stat (kit:nint (- curr d)) stats))
        (xtime (maps:get #"xtime" shift))
        (xd (kit:bin2int (maps:get #"xduration" shift))))
    (case xtime
      (#"false" (funcall next stats2))
      (#"true"
        (let* ((total_extra_time_w (maps:get #"total_extra_time_w" stats2))
              (total_std_time_w (maps:get #"total_std_time_w" stats2))
              (stats3 (maps:put #"total_extra_time_w" (- total_extra_time_w xd) stats2))
              (stats4 (maps:put #"total_std_time_w" (+ total_std_time_w (- d xd)) stats3)))
          (funcall next stats4))))))


(defun remove_shift_from_user_scheduled_stats (s shift)
  (remove_shift_from_user_stats s #"scheduled" shift))

(defun remove_shift_from_user_published_stats(s shift)
  (remove_shift_from_user_stats s #"published" shift))

(defun remove_shift_from_user_stats (s t shift)
  (case (maps:get #"status" shift)
    (#"unassigned" )
    (#"assigned"
      (users:find_by_id (maps:get #"assignedto" shift) (lambda (u)
        (let ((y (kit:bin2int (maps:get #"year" s)))
              (w (kit:bin2int (maps:get #"week" s)))
              (d (kit:bin2num (maps:get #"duration" shift))))
          (stats:with_weekly_stats u t y w (lambda (stats)
            (remove_shift_duration_from_weekly_stats shift stats (lambda (stats2)
              (stats:update_weekly_stats u t y w stats2 (lambda (_) ))))))))))))

(defun add_shift_to_user_published_stats (s shift)
  (add_shift_to_user_stats s #"published" shift))

(defun add_shift_to_user_scheduled_stats (s shift)
  (add_shift_to_user_stats s #"scheduled" shift))

(defun add_shift_to_user_stats (s t shift)
  (case (maps:get #"status" shift)
    (#"unassigned" )
    (#"assigned"
      (users:find_by_id (maps:get #"assignedto" shift) (lambda (u)
        (let ((y (kit:bin2int (maps:get #"year" s)))
              (w (kit:bin2int (maps:get #"week" s)))
              (d (kit:bin2num (maps:get #"duration" shift))))
          (stats:with_weekly_stats u t y w (lambda (stats)
            (add_shift_duration_to_weekly_stats shift stats (lambda (stats2)
              (stats:update_weekly_stats u t y w stats2 (lambda (_) ))))))))))))
                        
(defun clear_schedule (s next) 
  (with_schedule_shifts s (lambda (shifts)
    (lists:map (lambda (shift) 
      (remove_shift_from_user_published_bag s shift)
      (remove_shift_from_user_scheduled_bag s shift)
      (notify_shift_unassigned s shift (lambda () ))
      (remove_shift_from_user_scheduled_stats s shift )
      (remove_shift_from_user_published_stats s shift )
      (purge_shift shift) ) shifts)
    (db:remove_children s #"shifts" (lambda ()
      (with_schedule_empty_stats s (lambda (stats)
        (set_schedule_stats s stats (lambda (_)
          (clear_constraints s (lambda ()
            (clear_participant_stats s (lambda ()
              (funcall next))))))))))))))
     
(defun purge_shift (shift)
  (db:del (list #"shift" (maps:get #"id" shift))))

(defun set_schedule_status (u s status next)
  (set_schedule_status s status next))

(defun set_schedule_status (s status next)
  (common:set_status #"schedules" #"schedule" s status next ))

(defun with_schedule_template_shifts (s next)
  (with_template (maps:get #"template" s) (lambda (t)
    (with_template_shifts t next ))))

(defun shift_key (y m d sh sm)
  (kit:joinbin (list 
    (kit:int2bin y)
    (kit:datefield2bin m)
    (kit:datefield2bin d)
    (kit:datefield2bin sh)
    (kit:datefield2bin sm))))

(defun shift_end_key (shift) 
  (shift_key 
    (kit:bin2int (maps:get #"year" shift))
    (kit:bin2int (maps:get #"month" shift))
    (kit:bin2int (maps:get #"day" shift))
    (kit:bin2int (maps:get #"end_hour" shift))
    (kit:bin2int (maps:get #"end_min" shift))))

(defun create_schedule_shift (s tshift name tags type y m d sd sh sm ed eh em next)
  (let ((id (kit:uuid))
        (owner (maps:get #"ownedby" s))
        (schedule (maps:get #"id" s)) 
        (template (maps:get #"template" tshift))
        (tshid (maps:get #"id" tshift))
        (duration (shift_duration sd sh sm ed eh em))
        (key (shift_key y m d sh sm))
        (org (maps:get #"organization" s)))
    (db:hmset (list #"shift" id)
      (map #"id" id #"organization" org #"schedule" schedule #"template" template #"shift" tshid #"name" name #"published" #"false" #"status" #"unassigned" #"type" type #"year" y #"month" m #"day" d 
        #"start_day" sd #"end_day" ed #"start_hour" sh #"end_hour" eh #"start_min" sm #"end_min" em 
        #"created" (kit:now) #"createdby" owner #"ownedby" owner #"duration" duration #"key" key #"conflict" #"false" #"xtime" #"false" #"xduration" 0) (lambda(shift)
          (db:add_child s shift #"shifts" (lambda () 
            (common:add_tags shift tags (lambda (_) 
              (funcall next shift)))))))))

(defun create_availability_shift (a tshift type y m d sd sh sm ed eh em next)
  (let ((tid (maps:get #"template" tshift))
        (tshid (maps:get #"id" tshift)))
    (create_standalone_availability_shift a type y m d sd sh sm ed eh em (map #"template" tid #"tshift" tshid) 'false next))) 
    
(defun create_standalone_availability_shift (a type y m d sd sh sm ed eh em extra check_overlap next)
  (let* ((id (kit:uuid))
         (aid (maps:get #"id" a)) 
         (owner (maps:get #"ownedby" a))
         (oid (maps:get #"organization" a))
         (key (shift_key y m d sh sm))
         (duration (kit:int2bin (shift_duration sd sh sm ed eh em )))
         (s (maps:merge (map #"id" id #"organization" oid #"availability" aid #"type" #"available"  #"year" y #"month" m #"day" d 
                 #"start_day" (kit:int2bin sd) #"start_hour" (kit:int2bin sh) #"start_min" (kit:int2bin sm)
                 #"end_day" (kit:int2bin ed) #"end_hour" (kit:int2bin eh) #"end_min" (kit:int2bin em) 
                 #"created" (kit:now) #"createdby" owner #"ownedby" owner #"duration" duration #"key" key ) extra )))
    (case check_overlap
      ('true
        (with_availability_shifts a (lambda (shifts)
          (no_overlap? (sort_shifts (cons s shifts)) (lambda ()
            (db:hmset (list #"shift" id) s (lambda (s2)
              (db:add_child a s2 #"shifts" (lambda () 
                (funcall next s2))))))))))
      ('false
          (db:hmset (list #"shift" id) s (lambda (s2)
            (db:add_child a s2 #"shifts" (lambda () 
              (funcall next s2)))))))))
  
(defun create_standalone_schedule_shift (u s name type y m d sd sh sm ed eh em next)
  (let ((id (kit:uuid))
        (owner (maps:get #"id" u))
        (sid (maps:get #"id" s))
        (oid (maps:get #"organization" s))
        (key (shift_key y m d sh sm))
        (duration (shift_duration sd sh sm ed eh em)))
    (db:hmset (list #"shift" id)
      (map #"id" id #"schedule" sid #"organization" oid #"name" name  #"published" #"false" #"status" #"unassigned" #"type" type #"year" y #"month" m #"day" d 
        #"start_day" sd #"end_day" ed #"start_hour" sh #"end_hour" eh #"start_min" sm #"end_min" em 
        #"created" (kit:now) #"createdby" owner #"ownedby" owner #"duration" duration #"key" key #"conflict" #"false" #"xtime" #"false" #"xduration" 0) (lambda(shift)
          (db:add_child s shift #"shifts" (lambda () 
            (set_schedule_status u s #"draft" (lambda (s2)
              (common:with_access u shift next)))))))))

;; Assigns a user to a schedule shift 
;; (a) if the shift was assigned, then unassign the shift
;; (b) notify the new assignee only if the shift is assigned 
;; (c) no need to modify the schedule status
;;
(defun assign_schedule_shift (u s shift uid next)
  (case (maps:get #"status" shift)
    (#"assigned" 
      (let ((old_assignee (maps:get #"assignedto" shift)))
        (unassign_schedule_shift u s shift old_assignee (lambda (shift2)
          (assign_schedule_shift u s shift2 uid next)))))
    (#"unassigned" 
      (add_shift_to_user_scheduled_bag s shift uid (lambda ()
        (case (maps:get #"published" shift)
          (#"true" 
            (add_shift_to_user_published_bag s shift uid (lambda ()
              (set_shift_assigned shift uid (lambda (shift2)
                (users:find_by_id uid (lambda (assignee)
                  (notifications:shift assignee #"shift_assigned" shift2 (lambda ()
                    (funcall next shift2))))))))))
          (#"false" (set_shift_assigned shift uid next))))))))

    
;; Unassigns a user from a schedule shift
;; (a) remove the shift from the user bags
;; (b) notify the old assignee only if the shift was published
;; (c) clear the shift's assignedto field
;;
(defun unassign_schedule_shift (u s shift uid next)
  (remove_shift_from_user_scheduled_bag s shift uid (lambda ()
    (case (maps:get #"published" shift)
      (#"true" 
        (remove_shift_from_user_published_bag s shift uid (lambda ()
          (set_shift_unassigned shift (lambda (shift2)
            (users:find_by_id uid (lambda (assignee)
              (notifications:shift assignee #"shift_unassigned" shift2 (lambda () 
                (funcall next shift2 ))))))))))
      (#"false" (set_shift_unassigned shift next))))))

;;
;; Assign the specified schedule to the given
;; user id - with callback
;;
(defun assign_shift (s shift uid next)
  (db:hmset (list #"shift" (maps:get #"id" shift)) (map #"status" #"assigned" #"assignedto" uid) (lambda (shift2)
    (add_shift_to_user_scheduled_bag s shift uid (lambda () 
      (funcall next shift2))))))

;;
;; Assign the specified schedule to the given
;; user id - returns the shift
;;
(defun assign_shift (s shift uid) 
  (assign_shift s shift uid (lambda (s) s )))

(defun add_shift_to_user_published_bag (s shift uid next)
  (add_shift_to_user_bag s shift #"shifts-published" uid next))

(defun add_shift_to_user_scheduled_bag (s shift uid next)
  (add_shift_to_user_bag s shift #"shifts-scheduled" uid next))

(defun remove_shift_from_user_scheduled_bag (s shift uid next)
  (remove_shift_from_user_bag s shift #"shifts-scheduled" uid next))

(defun remove_shift_from_user_scheduled_bag(s shift uid)
  (remove_shift_from_user_scheduled_bag s shift uid (lambda () )))

(defun remove_shift_from_user_published_bag (s shift uid next)
  (remove_shift_from_user_bag s shift #"shifts-published" uid next))

(defun remove_shift_from_user_published_bag (s shift uid)
  (remove_shift_from_user_published_bag s shift uid (lambda () )))

(defun add_shift_to_user_bag (s shift bag uid next)
  (let ((id (maps:get #"id" shift))
        (week (maps:get #"week" s))
        (day (maps:get #"day" shift))
        (month (maps:get #"month" shift))
        (year (maps:get #"year" shift)))
    (db:sadd (list bag #"d" uid year month day) (list id) (lambda (_)
      (db:sadd (list bag #"w" uid year week) (list id) (lambda (_)
        (db:sadd (list bag #"m" uid year month) (list id) (lambda (_)
          (funcall next)))))))))

(defun remove_shift_from_user_bag (s shift bag uid next)
  (let ((id (maps:get #"id" shift))
        (week (maps:get #"week" s))
        (day (maps:get #"day" shift))
        (month (maps:get #"month" shift))
        (year (maps:get #"year" shift)))
    (db:srem (list bag #"d" uid year month day) (list id) (lambda (_)
      (db:srem (list bag #"w" uid year week) (list id) (lambda (_)
        (db:srem (list bag #"m" uid year month) (list id) (lambda (_)
          (funcall next)))))))))

(defun remove_shift_from_user_published_bag (s shift)
  (case (maps:get #"status" shift)
    (#"unassigned" )
    (#"assigned" 
      (remove_shift_from_user_published_bag s shift (maps:get #"assignedto" shift)))))

(defun remove_shift_from_user_scheduled_bag (s shift)
  (case (maps:get #"status" shift)
    (#"unassigned" )
    (#"assigned" 
      (remove_shift_from_user_scheduled_bag s shift (maps:get #"assignedto" shift)))))


(defun set_shift_unassigned (shift next)
  (db:hmset (list #"shift" (maps:get #"id" shift))
    (map #"assignedto" #"" #"status" #"unassigned") next))

(defun set_shift_assigned (shift uid next)
  (db:hmset (list #"shift" (maps:get #"id" shift))
    (map #"assignedto" uid #"status" #"assigned") next))
  
(defun with_schedule_shift (u shid next)
  (db:hgetall (list #"shift" shid) (lambda (shift)
    (common:with_access u shift next))))

(defun with_schedule_shift (id next)
  (db:hgetall (list #"shift" id) next)) 
 
(defun with_schedule_shift_or_empty(id next)
  (case (db:hgetall (list #"shift" id))
    ((tuple 'error _) (funcall next #M()))
    ((= _ s) (funcall next s))))

(defun update_schedule_shift (u s shift name type y m d sd sh sm ed eh em next)
    (case (maps:get #"status" shift)
      (#"unassigned" 
        (update_schedule_shift shift name type y m d sd sh sm ed eh em (lambda (s2)
          (set_schedule_status u s #"draft" (lambda (_)
            (common:with_access u s2 next))))))
      (#"assigned"
        (let ((uid (maps:get #"assignedto" shift))
              (shift2 (maps:merge shift (map #"year" (kit:int2bin y) #"month" (kit:int2bin m) #"day" (kit:int2bin d)))))
          (remove_shift_from_user_scheduled_bag s shift uid (lambda ()
            (add_shift_to_user_scheduled_bag s shift2 uid (lambda ()
              (case (maps:get #"published" shift)
                (#"false" (update_schedule_shift shift name type y m d sd sh sm ed eh em next))
                (#"true" 
                  (remove_shift_from_user_published_bag s shift uid (lambda ()
                    (add_shift_to_user_published_bag s shift2 uid (lambda () 
                      (update_schedule_shift shift name type y m d sd sh sm ed eh em (lambda(shift3)
                        (set_schedule_status u shift3 #"draft" (lambda (_)
                          (common:with_access u shift3 (lambda (shift4)
                            (users:find_by_id uid (lambda (assignee)
                              (notifications:shift assignee #"shift_modified" shift4 (lambda ()
                                (funcall next shift4)))))))))))))))))))))))))

(defun update_schedule_shift (shift name type y m d sd sh sm ed eh em next)
  (let ((key (shift_key y m d sh sm)))
    (db:hmset (list #"shift" (maps:get #"id" shift))
      (map #"name" name #"type" type #"year" y #"month" m #"day" d 
        #"start_day" sd #"end_day" ed #"start_hour" sh #"end_hour" eh #"start_min" sm #"end_min" em #"duration" (shift_duration sd sh sm ed eh em)) next)))

(defun copy_schedule_shift (u s shift name type y m d sd sh sm ed eh em next)
  (create_standalone_schedule_shift u s name type y m d sd sh sm ed eh em (lambda (s2)
    (common:with_tags shift (lambda (tags)
      (common:add_tags s2 tags (lambda (_)
        (case (maps:get #"status" shift)
          (#"unassigned" (funcall next s2))
          (#"assigned"
            (let ((uid (maps:get #"assignedto" shift)))
              (assign_schedule_shift u s s2 uid next))))))))))) 

(defun remove_schedule_shift(u s shift next) 
  (db:del (list #"shift" (maps:get #"id" s)) (lambda ()
    (db:remove_child s shift #"shifts" (lambda ()
      (set_schedule_status u s #"draft" (lambda (s2) 
        (case (maps:get #"status" shift)
          (#"unassigned" (funcall next))
          (#"assigned"
            (remove_shift_from_user_scheduled_bag s shift)
            (case (maps:get #"published" shift)
              (#"false" (funcall next))
              (#"true"
                (remove_shift_from_user_scheduled_bag s shift)
                (users:find_by_id (maps:get #"assignedto" shift) (lambda (assignee)
                  (notifications:shift assignee #"shift_unassigned" shift (lambda ()
                    (funcall next ))))))))))))))))

(defun create_schedule_shift
  ((0 s tshift name tags type y m d sd sh sm ed eh em ids next) 
    (funcall next ids))
  ((r s tshift name tags type y m d sd sh sm ed eh em ids next)
    (create_schedule_shift s tshift name tags type y m d sd sh sm ed eh em (lambda (shift)
      (create_schedule_shift (- r 1) s tshift name tags type y m d sd sh sm ed eh em (cons (maps:get #"id" shift) ids) next )))))
 
(defun unwind_template_shift (tshift sid next)
  (with_schedule sid (lambda (s)
    (common:with_tags tshift (lambda (tags)
      (let* ((staffing (kit:bin2int (maps:get #"staffing" tshift)))
            (week (kit:bin2int (maps:get #"week" s)))
            (year (kit:bin2int (maps:get #"year" s)))
            (sd (kit:bin2int (maps:get #"start_day" tshift)))
            (ed (kit:bin2int (maps:get #"end_day" tshift)))
            ((tuple _ m d) (kit:goto_week_day year week sd))
            (sh (kit:bin2int (maps:get #"start_hour" tshift)))
            (eh (kit:bin2int (maps:get #"end_hour" tshift)))
            (sm (kit:bin2int (maps:get #"start_min" tshift)))
            (em (kit:bin2int (maps:get #"end_min" tshift)))
            (name (maps:get #"name" tshift))
            (type (maps:get #"type" tshift)))
        (create_schedule_shift staffing s tshift name tags type year m d sd sh sm ed eh em '() next)))))))          

(defun create_availability_shift (tshift a next)
  (let* ((week (kit:bin2int (maps:get #"week" a)))
         (year (kit:bin2int (maps:get #"year" a)))
         (sd (kit:bin2int (maps:get #"start_day" tshift)))
         (ed (kit:bin2int (maps:get #"end_day" tshift)))
         ((tuple _ m d) (kit:goto_week_day year week sd))
         (sh (kit:bin2int (maps:get #"start_hour" tshift)))
         (eh (kit:bin2int (maps:get #"end_hour" tshift)))
         (sm (kit:bin2int (maps:get #"start_min" tshift)))
         (em (kit:bin2int (maps:get #"end_min" tshift)))
         (type (maps:get #"type" tshift)))
    (create_availability_shift a tshift type year m d sd sh sm ed eh em next)))      

(defun create_availability_shifts 
  (('() a next)(funcall next))
  (((cons s rest) a next)
    (create_availability_shift s a (lambda(_)
      (create_availability_shifts rest a next)))))

(defun shift_props () 
  (list #"id" #"name" #"assignedto" #"organization" #"schedule" #"template" #"shift" #"status" #"created" #"ownedby" #"createdby" #"year" #"month" #"day" 
    #"start_day" #"start_hour" #"start_min" #"end_day" #"end_hour" #"end_min" #"type" #"published" #"duration" #"key" #"conflict" #"xtime" #"xduration" ))

(defun with_shifts (k next)
  (db:join k #"shift" (shift_props) #"key" #"ASC" next))
  
(defun with_schedule_shifts (u s next)
  (with_schedule_shifts s next))

(defun with_schedule_shifts (s next)
  (with_shifts (list #"shifts" (maps:get #"id" s)) next))

(defun with_schedule_participants (shifts next)
  (with_schedule_participants shifts '() next))

(defun with_schedule_participants
  (('() participants next)(funcall next (lists:map (lambda (id) (users:resolve id)) (kit:luniq participants))))
  (((cons s rest) participants next)
    (case (maps:get #"status" s)
      (#"unassigned" (with_schedule_participants rest participants next))
      (#"assigned" (with_schedule_participants rest (cons (maps:get #"assignedto" s) participants)  next)))))

(defun start_engine (u s next)
  (set_schedule_status u s #"running" (lambda (s2)
    (funcall next s2))))

;;
;; Creates empty stats 
;;
(defun with_schedule_empty_stats (s next )
  (db:hmset (list #"stats" (maps:get #"id" s))
    (map #"quality" 0
      #"staffing" 0
      #"staffing_total" 0
      #"hard" 0
      #"hard_total" 0
      #"soft" 0
      #"soft_total" 0) next))

;;
;;
;;
(defun set_schedule_stats (s stats next)
  (db:hmset (list #"stats" (maps:get #"id" s)) (maps:put #"schedule" (maps:get #"id" s) stats) next))

;;
;; Returns the stats for the given
;; schedule - with callback
;;
(defun with_schedule_stats (u s next)
  (with_schedule_stats s next))  

;;
;; Returns the stats for the given
;; schedule - no callback
;;
(defun with_schedule_stats (s next)
  (common:with_stats s next))

;; 
;; Evaluates the specified set of profile rules,
;; given the specified set of shifts and initial stats
;; - returns the number of verified strong and weak rules,
;; and the new stats
;;
(defun eval_rules (rules shifts stats) 
  (with_durations_into_stats shifts stats (lambda (stats2)
    (eval_rules (sort_rules (resolve_rules (filter_rules rules))) (sort_shifts shifts) stats2 0 0 0 '() '()))))




;;
;; Sorts rules in a proper way, in order to resolve inter-dependencies
;;
(defun sort_rules (rules) 
  (lists:sort (lambda (a b) (< (maps:get #"rank" a) (maps:get #"rank" b) )) rules))

(defun filter_rules (rules)
  (filter_rules rules #"status" #"active"))

(defun filter_rules (rules prop value) 
  (lists:filter (lambda (r) (== value (maps:get prop r))) rules)) 

(defun resolve_rules (rules) 
  (lists:map #'resolve_rule/1 rules ))

;;
;; Recursively evaluate rules 
;;
(defun eval_rules
  ((() shifts stats hard soft violations ver nver) 
    (tuple hard soft violations stats ver nver))
  (((cons r next_rules) shifts stats hard soft violations ver nver)
    (let (((tuple h s v stats2 ver2 nver2)(eval_rule r shifts stats ver nver)))
      (eval_rules next_rules shifts stats2 (+ hard h) (+ soft s) (+ violations v) ver2 nver2))))

;;
;; Merges the info from the rule and the indicator into a 
;; single convenient map
;; 
(defun resolve_rule (r)
  (with_indicator (maps:get #"indicator" r) (lambda (i)
    (maps:merge r
      (map #"indicator" (maps:get 'name i) 
           #"unit" (maps:get 'unit i)
           #"type" (maps:get 'type i)
           #"strong" (kit:bin2bool (maps:get #"strong" r))
           #"rank" (maps:get 'rank i)
           #"value" (kit:bin2num (maps:get #"value" r)))))))

;;
;; Evaluate one rule against the set of shifts and 
;; using the given stats as reference
;;
(defun eval_rule (r shifts stats ver nver)
  (eval_rule 
    (maps:get #"indicator" r) 
    (maps:get #"unit" r)
    (maps:get #"type" r)
    (maps:get #"strong" r) 
    (maps:get #"value" r) shifts stats ver nver))


(defun with_durations_into_stats (shifts stats next) 
  (let* ((c_std_time_w (maps:get #"total_std_time_w" stats))
         (s_std_time_w (visit_shifts shifts #"std" #"duration" 0))
         (t_std_time_w (+ c_std_time_w s_std_time_w))
         (c_idle_time_w (maps:get #"total_idle_time_w" stats))
         (s_idle_time_w (visit_shifts shifts #"idle" #"duration" 0))
         (t_idle_time_w (+ c_idle_time_w s_idle_time_w))
         (c_training_time_w (maps:get #"total_training_time_w" stats))
         (s_training_time_w (visit_shifts shifts #"training" #"duration" 0))
         (t_training_time_w (+ c_training_time_w s_training_time_w)))
    (funcall next (maps:merge stats (map
      #"total_std_time_w" t_std_time_w 
      #"s_std_time_w" s_std_time_w
      #"total_idle_time_w" t_idle_time_w 
      #"s_idle_time_w" s_idle_time_w
      #"total_training_time_w" t_training_time_w 
      #"s_training_time_w" s_training_time_w)))))


;;
;; Specific rule evaluation
;;
(defun eval_rule
  
  (('std_time_w unit type strong v shifts stats ver nver) 
    (let* ((t_std_time_w (maps:get #"total_std_time_w" stats))
           (ref (* 60 v)) 
           (stats2 (maps:put #"max_std_time_w" ref stats))
           (verified (>= t_std_time_w ref))
           (violations (kit:bool2int verified)))
      (rule_result 'std_time_w unit type strong ref t_std_time_w verified violations stats2 ver nver)))   
  
  (('idle_time_w unit type strong v shifts stats ver nver)
    (let* ((t_idle_time_w (maps:get #"total_idle_time_w" stats))
           (ref (* 60 v))
           (stats2 (maps:put #"min_idle_time_w" ref stats))
           (verified (>= t_idle_time_w ref))
           (violations (kit:bool2int verified)))
      (rule_result 'idle_time_w unit type strong ref t_idle_time_w verified violations stats2 ver nver)))   
 
  (('training_time_w unit type strong v shifts stats ver nver)
    (let* ((t_training_time_w (maps:get #"total_training_time_w" stats))
           (ref (* 60 v))
           (stats2 (maps:put #"min_training_time_w" ref stats))
           (verified (>= t_training_time_w ref))
           (violations (kit:bool2int verified)))
      (rule_result 'training_time_w unit type strong ref t_training_time_w verified violations stats2 ver nver)))   

  (('extra_time_w unit type strong v shifts stats ver nver)
    (let* ((max_std_time_w (maps:get #"max_std_time_w" stats))
           (t_std_time_w (maps:get #"total_std_time_w" stats))
           (s_std_time_w (maps:get #"s_std_time_w" stats))
           (s_extra_time_w (kit:nint (- s_std_time_w max_std_time_w)))
           (t_extra_time_w (kit:nint (- t_std_time_w max_std_time_w)))
           (ref (* 60 v))
           (verified (>= ref s_extra_time_w))
           (stats2 (maps:merge stats (map #"total_extra_time_w" t_extra_time_w 
                                          #"s_extra_time_w" s_extra_time_w
                                          #"total_std_time_w" ( - t_std_time_w t_extra_time_w )
                                          #"s_std_time_w" ( - s_std_time_w s_extra_time_w ))))
           (violations (kit:bool2int verified)))
      (rule_result 'extra_time_w unit type strong ref s_extra_time_w verified violations stats2 ver nver)))
      
  (('min_scheduled_time_d unit type strong v shifts stats ver nver)
    (let* ((stats2 (daily_scheduled_time shifts stats))
           (ref (* 60 v))
           (shifts_d (shifts_by_day shifts))
           (scheduled_time_d (map_days (lambda (ds)(reduce_day 'sum #"duration" ds)) shifts_d))
           (min_value (reduce_days 'min scheduled_time_d))
           (verified_d (map_days 'gte ref scheduled_time_d))
           (violations_d (map_days (lambda (day) (kit:bool2int (not day))) verified_d))
           (violations (reduce_days 'sum violations_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'min_scheduled_time_d unit type strong ref min_value verified violations stats2 ver nver)))

  (('max_scheduled_time_d unit type strong v shifts stats ver nver)
    (let* ((stats2 (daily_scheduled_time shifts stats))
           (ref (* 60 v))
           (shifts_d (shifts_by_day shifts))
           (scheduled_time_d (map_days (lambda (ds)(reduce_day 'sum #"duration" ds)) shifts_d))
           (max_value (reduce_days 'max scheduled_time_d))
           (verified_d (map_days 'lte ref scheduled_time_d))
           (violations_d (map_days (lambda (day) (kit:bool2int (not day))) verified_d))
           (violations (reduce_days 'sum violations_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'max_scheduled_time_d unit type strong ref max_value verified violations stats2 ver nver)))
  
  (('min_time_between_shifts_d unit type strong v shifts stats ver nver)
    (let* ((shifts_d (shifts_by_day shifts))
           (ref v)
           (f_shifts_d (filter_days (lambda (day) (> (reduce_day 'count day) 1)) shifts_d))
           (times_d (map_days (lambda (ds) (times_between_shifts ds)) f_shifts_d))
           (violations_d (map_days (lambda (day) (reduce_day 'lt ref day)) times_d))
           (violations (reduce_days 'sum violations_d))
           (min_times_d (map_days 'min times_d))
           (min_value (reduce_days 'min min_times_d))
           (verified_d (map_days 'gte v min_times_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'min_time_between_shifts_d unit type strong ref min_value verified violations stats ver nver)))
  
  (('max_time_between_shifts_d unit type strong v shifts stats ver nver)
    (let* ((shifts_d (shifts_by_day shifts))
           (ref v)
           (f_shifts_d (filter_days (lambda (day) (> (reduce_day 'count day) 1)) shifts_d))
           (times_d (map_days (lambda (ds) (times_between_shifts ds)) f_shifts_d))
           (violations_d (map_days (lambda (day) (reduce_day 'gt ref day)) times_d))
           (violations (reduce_days 'sum violations_d))
           (max_times_d (map_days 'max times_d))
           (max_value (reduce_days 'max max_times_d))
           (verified_d (map_days 'lte v max_times_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'max_time_between_shifts_d unit type strong ref max_value verified violations stats ver nver)))
  
  (('min_shift_duration_d unit type strong v shifts stats ver nver)
    (let* ((shifts_d (shifts_by_day shifts))
           (ref v)
           (times_d (map_days (lambda (ds) (shifts_durations ds)) shifts_d))
           (violations_d (map_days (lambda (day) (reduce_day 'lt ref day)) times_d))
           (violations (reduce_days 'sum violations_d))
           (min_times_d (map_days 'min times_d))
           (min_value (reduce_days 'min min_times_d))
           (verified_d (map_days 'gte v min_times_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'min_shift_duration_d unit type strong ref min_value verified violations stats ver nver)))
    
  (('max_shift_duration_d unit type strong v shifts stats ver nver)
    (let* ((shifts_d (shifts_by_day shifts))
           (ref v)
           (times_d (map_days (lambda (ds) (shifts_durations ds)) shifts_d))
           (violations_d (map_days (lambda (day) (reduce_day 'gt ref day)) times_d))
           (violations (reduce_days 'sum violations_d))
           (max_times_d (map_days 'max times_d))
           (max_value (reduce_days 'max max_times_d))
           (verified_d (map_days 'lte v max_times_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'max_shift_duration_d unit type strong ref max_value verified violations stats ver nver)))
 
  (('max_allowed_shift_overlaps_d unit type strong v shifts stats ver nver)
    (let* ((shifts_d (shifts_by_day shifts))
           (ref v)
           (conflicts_d (map_days (lambda (ds) (count_overlaps ds)) shifts_d))
           (max_value (reduce_days 'max conflicts_d))
           (verified_d (map_days 'lte v conflicts_d))
           (violations_d (map_days (lambda (day) (kit:bool2int (not day))) verified_d))
           (violations (reduce_days 'sum violations_d))
           (verified (verified (reduce_days 'true verified_d))))
      (rule_result 'max_allowed_shift_overlaps_d unit type strong ref max_value verified violations stats ver nver)))

  )

(defun no_overlap? (shifts next)
  (let* ((shifts_d (shifts_by_day shifts))
         (conflicts_d (map_days (lambda (ds) (count_overlaps ds)) shifts_d))
         (max_value (reduce_days 'max conflicts_d))
         (verified_d (map_days 'lte 0 conflicts_d))
         (verified (verified (reduce_days 'true verified_d))))
    (case verified
      ('false (kit:err 'conflict 'overlap))
      ('true (funcall next)))))

(defun shifts_by_day (shifts)
  (map_days (lambda (day) (lists:reverse day)) 
    (lists:foldl (lambda (s acc) (kit:mladd (maps:get #"start_day" s) s acc)) #M() shifts)))

(defun reduce_day
  (('gt ref values)
   (lists:foldl (lambda (v acc)
      (case (> v ref) 
        ('true (+ acc v))
        ('false acc))) 0 values))
  (('lt ref values)
   (lists:foldl (lambda (v acc)
      (case (< v ref) 
        ('true (+ acc v))
        ('false acc))) 0 values))
  (('sum k shifts)
    (lists:foldl (lambda (s acc) (+ acc (kit:bin2int (maps:get k s)))) 0 shifts)))

(defun reduce_day
  (('count shifts) (length shifts)))


(defun map_days 
  (('min days)
    (map_days (lambda (day) (lists:min day)) days))
  (('max days)
    (map_days (lambda (day) (lists:max day)) days))
  ((fn days)
    (maps:map (lambda (day shifts) (funcall fn shifts)) days)))

(defun map_days
  (('lte ref days)(map_days (lambda (v) (=< v ref)) days))
  (('gte ref days)(map_days (lambda (v) (>= v ref)) days)))

(defun reduce_days
  (('sum days) 
    (reduce_days (lambda (all) (lists:sum all)) 0 days))
  (('min days) 
    (reduce_days (lambda (all) (lists:min all)) 'undefined days))
  (('max days) 
    (reduce_days (lambda (all) (lists:max all)) 'undefined days))
  (('true days)
    (reduce_days (lambda (all) (lists:all (lambda (v) (== 'true v)) all)) 'undefined days)))

(defun reduce_days (fn default days)
  (case (maps:size days)
    (0 default)
    (_ (funcall fn (maps:values days)))))

(defun filter_days (fn days)
  (maps:filter (lambda (d day)(funcall fn day)) days))

(defun normalize_days (days)
  (maps:merge (map #"0" 'true #"1" 'true #"2" 'true #"3" 'true #"4" 'true #"5" 'true #"6" 'true) days)) 

(defun verified (v)
  (case v
    ('undefined 'true)
    (_ v)))

(defun daily_scheduled_time (shifts stats)
  (lists:foldl (lambda (s acc)
    (kit:new_counters acc
      (map (kit:catbin (list #"scheduled_time_d_" (maps:get #"start_day" s))) (kit:bin2int (maps:get #"duration" s)))))
  stats shifts))

(defun times_between_shifts (shifts)
  (case (length shifts)
    (0 (list 0))
    (1 (list 0))
    (_ (times_between_shifts shifts 'undefined '()))))

(defun shifts_durations (shifts)
  (case (length shifts)
    (0 0)
    (_ (lists:map (lambda (s) (kit:bin2int (maps:get #"duration" s))) shifts))))

(defun times_between_shifts
  (('() _ times ) times)
  (((cons s1 r) 'undefined times) (times_between_shifts r s1 times))
  (((cons s2 r) s1 times)(times_between_shifts r s2 (cons (time_between_shifts s1 s2) times))))

(defun time_between_shifts (s1 s2)
  (kit:nint (- (shift_start_minute s2) (shift_end_minute s1))))
  
(defun is_shift_same_day (s2 s1)
 (and (and (== (kit:bin2num (maps:get #"year" s2)) (kit:bin2num (maps:get #"year" s1))) 
       (== (kit:bin2num (maps:get #"month" s2)) (kit:bin2num (maps:get #"month" s1)))) 
       (== (kit:bin2num (maps:get #"day" s2)) (kit:bin2num (maps:get #"day" s1)))))

(defun is_shift_same_start_day (s2 s1)
  (== (maps:get #"start_day" s2) (maps:get #"start_day" s1)))

(defun shift_start_minute (s)
  (+ (* 60 (kit:bin2num (maps:get #"start_hour" s))) (kit:bin2num (maps:get #"start_min" s))))

(defun shift_end_minute (s)
  (+ (* 60 (kit:bin2num (maps:get #"end_hour" s))) (kit:bin2num (maps:get #"end_min" s))))


(defun count_overlaps (shifts)
  (let (((tuple c _)(lists:foldl (lambda (s acc)
    (case acc
      (0 (tuple 0 s))
      ((tuple overlaps s0)
        (case (same_day_shift_overlap? s0 s)
          ('true (tuple (+ 1 overlaps) s))
          ('false (tuple overlaps s)))))) 0 shifts))) c))

(defun apply_rule_to_shift (name rules shift stats next)
  (apply_rule_to_shift_2 name (sort_rules (resolve_rules (filter_rules rules))) shift stats next))
    
(defun apply_rule_to_shift_2 
  ((_ '() shift stats next) (funcall next shift stats))
  ((n1 (cons r2 t) shift stats next) 
    (let (((tuple shift2 stats2) 
          (apply_rule_to_shift_3 n1 (maps:get #"indicator" r2) (maps:get #"value" r2) shift stats)))
      (apply_rule_to_shift_2 n1 t shift2 stats2 next))))
      
(defun apply_rule_to_shift_3
  (('extra_time_w 'std_time_w v shift stats)
    (let* ((max_std_time_w (* 60 v))
           (t_std_time_w (maps:get #"total_std_time_w" stats)))
      (case (maps:get #"type" shift)
        (#"std" 
          (case (> t_std_time_w max_std_time_w)
            ('true 
              (let* ((diff (- t_std_time_w max_std_time_w))
                     (stats2 (maps:put #"total_std_time_w" max_std_time_w stats))
                     (stats3 (maps:put #"total_extra_time_w" (+ diff (maps:get #"total_extra_time_w" stats2)) stats2)))
                (set_shift_extra_time shift diff (lambda (shift2)
                  (tuple shift2 stats3)))))
            ('false (tuple shift stats))))
        (_ (tuple shift stats)))))
  ((_ _ _ shift stats) (tuple shift stats)))
    

(defun num2bin
  (('undefined) 'undefined)
  (((= _ v)) (kit:num2bin v)))


;;
;; Convenience function to return
;; the result from a single rule evaluation
;;
(defun rule_result (ind unit type strong ref v res violations stats ver nver)
  (with_new_constraint ind unit type strong (kit:num2bin ref) (num2bin v) (lambda (c)
    (case res
      ('true (rule_result_to_q_delta strong res violations stats (cons c ver) nver))
      ('false (rule_result_to_q_delta strong res violations stats ver (cons c nver)))))))

(defun rule_result_to_q_delta (strong res violations stats ver nver)
  (let ((res_int (kit:bool2int res)))
    (case strong
      ('true (tuple res_int 0 violations stats ver nver))
      ('false (tuple 0 res_int violations stats ver nver)))))

(defun visit_shifts (shifts type prop acc0)
  (visit_shifts shifts type prop (lambda (s) 'true) acc0)) 

(defun visit_shifts (shifts type prop cond acc0) 
  (lists:foldl (lambda (s acc) 
    (case (tuple (== type (maps:get #"type" s)) (funcall cond s))
      ((tuple 'true 'true) (+ acc (kit:bin2num (maps:get prop s))))
      (_ acc ))) acc0 shifts))

;;
;; Publishes a schedule - this involes the following tasks:
;; (a) setting the schedule status to published
;; (b) publish every shift
;;
(defun publish (u s next) 
  (with_schedule_shifts u s (lambda (shifts)
    (lists:map 
      (lambda (shift) 
        (publish_shift u s shift (lambda (shift2)
          (add_shift_to_user_published_stats s shift2))))
      (lists:filter (lambda (shift) 
        (and (== #"false" (maps:get #"published" shift))
             (== #"assigned" (maps:get #"status" shift)))) shifts))
    (set_schedule_status u s #"published" next ))))

(defun publish_shift (u s shift next) 
  (let ((year (maps:get #"year" shift))
        (month (maps:get #"month" shift))
        (week (maps:get #"week" s))
        (day (maps:get #"day" shift))
        (id (maps:get #"id" shift))
        (assignedto (maps:get #"assignedto" shift)))
    (add_shift_to_user_published_bag s shift assignedto (lambda ()    
      (db:hmset (list #"shift" id) (map #"published" #"true") (lambda (shift2)
        (users:find_by_id assignedto (lambda (assignee)
          (notifications:shift assignee #"shift_assigned" shift2 (lambda ()
            (notify_shift_conflict assignee shift2 (lambda () 
              (funcall next shift2)))))))))))))

(defun publish_shift (u s shift) 
  (publish_shift u s shift (lambda (shift2) shift2)))

(defun with_published_daily_shifts (u y m d next)
  (with_shifts (list #"shifts-published-d" (maps:get #"id" u) y m d) next))

(defun with_published_weekly_shifts (u y w next)
  (with_shifts (list #"shifts-published-w" (maps:get #"id" u) y w) next))

(defun with_published_monthly_shifts (u y m next)
  (with_shifts (list #"shifts-published-m" (maps:get #"id" u) y m) next))

(defun with_scheduled_daily_shifts (u y m d next)
  (with_shifts (list #"shifts-scheduled-d" (maps:get #"id" u) y m d) next))

(defun with_scheduled_weekly_shifts (u y w next)
  (with_shifts (list #"shifts-scheduled-w" (maps:get #"id" u) y w) next))

(defun with_scheduled_monthly_shifts (u y m next)
  (with_shifts (list #"shifts-scheduled-m" (maps:get #"id" u) y m) next))

(defun shift_start_datetime (s)
  (tuple 
    (tuple (kit:bin2int (maps:get #"year" s)) (kit:bin2int (maps:get #"month" s)) (kit:bin2int (maps:get #"day" s)))
    (tuple (kit:bin2int (maps:get #"start_hour" s)) (kit:bin2int (maps:get #"start_min" s)) 0)))

(defun shift_start_minute_of_day (s)
  (+ (* 60 (kit:bin2int (maps:get #"start_hour" s))) (kit:bin2int (maps:get #"start_min" s))))

(defun shift_overlap? (s1 s2)
  (let ((s1start (calendar:datetime_to_gregorian_seconds (shift_start_datetime s1)))
        (s2start (calendar:datetime_to_gregorian_seconds (shift_start_datetime s2)))
        (s1d (* 60 (kit:bin2int (maps:get #"duration" s1)))))
    (> (+ s1start s1d ) s2start)))

(defun shift_within_shift? (s1 s2)
  (let* ((s1start (shift_start_minute_of_day s1))
         (s2start (shift_start_minute_of_day s2))
         (s1end (+ s1start (kit:bin2int (maps:get #"duration" s1))))
         (s2end (+ s2start (kit:bin2int (maps:get #"duration" s2)))))
    (and (>= s1start s2start) (>= s2end s1end))))
  
(defun shift_within_shifts? (s shifts)
  (lists:foldl (lambda (s1 acc)
    (case acc
      ('true 'true)
      ('false (shift_within_shift? s s1)))) 'false shifts))


(defun same_day_shift_overlap? (s1 s2)
  (let ((s1start (shift_start_minute_of_day s1))
        (s2start (shift_start_minute_of_day s2)))
    (> (+ s1start (kit:bin2int (maps:get #"duration" s1))) s2start)))
  
(defun set_shift_conflict (s next)
  (db:hmset (list #"shift" (maps:get #"id" s)) (map #"conflict" #"true") next))
  
(defun set_shift_extra_time (s d next)
  (db:hmset (list #"shift" (maps:get #"id" s)) (map #"xtime" #"true" #"xduration" (kit:int2bin (trunc d))) next))

(defun sort_shifts (shifts)
  (sort_shifts shifts #'shift_start_key/1))

(defun sort_template_shifts (shifts)
  (sort_shifts shifts #'shift_start_minute_of_day/1))
  
(defun sort_shifts_by_prop (shifts prop)
  (sort_shifts shifts (lambda (s) (kit:bin2num (maps:get prop s)))))

(defun sort_shifts (shifts fun)
  (lists:sort (lambda (s1 s2)
    (=< (funcall fun s1) (funcall fun s2))) shifts))

(defun shift_start_key (s)
  (kit:bin2num (kit:catbin (list
    (kit:datefield2bin (kit:bin2int (maps:get #"start_day" s)))
    (kit:datefield2bin (kit:bin2int (maps:get #"start_hour" s)))
    (kit:datefield2bin (kit:bin2int (maps:get #"start_min" s)))))))

(defun set_schedule_auto_mode (u s auto next)
  (case (maps:get #"status" s)
    (#"running" (kit:err 'forbidden 'status))
    (#"queued" (kit:err 'forbidden 'status))
    (_
      (case (maps:is_key #"template" s)
        ('false (kit:err 'forbidden 'template))
        ('true 
          (db:hmset (list #"schedule" (maps:get #"id" s)) (map #"auto" auto) next))))))

(defun constraint_rank (strong)
  (case strong
    ('true 1)
    ('false 0)))

(defun with_new_constraint (ind unit type strong ref value next)
  (funcall next (map #"id" (kit:uuid) #"unit" unit #"rank" (constraint_rank strong) #"indicator" ind #"type" type #"strong" strong #"ref" ref #"value" value))) 

(defun add_verified_constraints (s constraints user next) 
  (add_constraints s #"verified" constraints user next))

(defun add_nonverified_constraints (s constraints user next) 
  (add_constraints s #"nonverified" constraints user next))

(defun add_constraints (s type constraints user next) 
  (lists:map (lambda (c) (add_constraint s type (maps:put #"user" user c))) constraints)
  (funcall next))

(defun add_constraint (s type c)
  (db:hmset (list #"constraint" (maps:get #"id" c)) c (lambda (_)   
    (db:add_child s c (kit:joinbin (list #"constraints" type ) #"-")))))

(defun clear_constraints (s next)
  (db:remove_children s #"constraints-verified" (lambda ()
    (db:remove_children s #"constraints-nonverified" next))))

(defun constraints_key (s v) 
  (case v 
    ('true (list #"constraints" #"verified" (maps:get #"id" s)))
    ('false (list #"constraints" #"nonverified" (maps:get #"id" s)))))

(defun with_constraints (s v next)
  (db:join
    (constraints_key s v)
    #"constraint"
    (list #"id" #"indicator" #"user" #"ref" #"value" #"strong" #"created" #"rank" #"type" #"unit" )
    #"rank"
    next))

(defun create_availability (u o y w next)
  (let* ((uid (maps:get #"id" u))
        (oid (maps:get #"id" o))
        (year (kit:int2bin y))
        (week (kit:int2bin w))
        (key (list #"availability" uid oid year week)))
    (db:setnx key (lambda (id)
      (db:hmset 
        (list #"availability" id) 
        (map #"id" id #"organization" oid #"template" #"undefined" #"status" #"enabled" #"dirty" #"false"
             #"year" year #"week" week #"created" (kit:now) #"ownedby" uid) (lambda (a)
          (db:add_child u a (kit:joinbin (list #"availabilities" year week) #"-" ) (lambda ()
            (common:with_access u a next)))))))))

(defun with_availability (u o y w next)
  (let* ((uid (maps:get #"id" u))
         (oid (maps:get #"id" o))
         (year (kit:int2bin y))
         (week (kit:int2bin w))
         (key (list #"availability" uid oid year week)))
    (case (db:get key)
      ((tuple 'error (tuple 'not_found _)) (funcall next (map #"status" #"disabled")))
      ((= _ id) (with_availability u id next))))) 

(defun with_availability (u id next)
  (db:hgetall (list #"availability" id) (lambda (a)
    (common:with_access u a next))))

(defun with_availabilities (u y w next) 
  (db:join_alpha
    (list #"availabilities" y w (maps:get #"id" u))
    #"availability"
    (list #"organization" #"id" #"template" #"status" #"year" #"week" #"created" #"ownedby" #"dirty" )
    #"organization"
    next))

(defun with_shifts_duration (shifts next)
  (funcall next (reduce_day 'sum #"duration" shifts))) 
  
(defun set_availability_template (u a t next)
  (db:hmset (list #"availability" (maps:get #"id" a) ) (map #"template" (maps:get #"id" t)) (lambda (a2)
    (common:with_access u a2 next))))
 
(defun enable_availability (u a next)
  (set_availability_property a #"status" #"enabled" (lambda (a2)
    (common:with_access u a2 next))))

(defun disable_availability (u a next)
  (set_availability_property a #"status" #"disabled" (lambda (a2)
    (common:with_access u a2 next))))

(defun edit_availability (u a next) 
  (set_availability_property a #"status" #"edit" (lambda (a2)
    (case (maps:get #"template" a2)
      (#"undefined" (common:with_access u a2 next))
      ((= _ tid)
        (with_availability_template u tid (lambda (t)
          (with_template_shifts t (lambda (shifts)
            (create_availability_shifts shifts a (lambda ()
              (common:with_access u a2 next))))))))))))

(defun remove_shifts (a shifts next)
  (lists:map (lambda (s) (purge_shift s)) shifts)
  (db:remove_children a #"shifts" next))


(defun is_same_shift (s shift)
  (== (maps:get #"id" s) (maps:get #"id" shift)))

(defun replace_shift (shift shifts)
  (let* ((shifts2 (lists:filter (lambda (s) (not (is_same_shift s shift))) shifts)))
    (cons shift shifts2)))

(defun discard_availability_changes (u a next)
  (with_availability_shifts u a (lambda (shifts) 
    (remove_shifts a shifts (lambda ()  
      (set_availability_properties a (map #"dirty" #"false" #"status" #"enabled") (lambda (a2)
        (common:with_access u a2 next))))))))

(defun with_availability_shift (a id next)
  (db:hgetall (list #"shift" id) next))



(defun with_availability_shifts (u a next)
  (with_availability_shifts a next))

(defun with_availability_shifts (a next)
  (case (maps:get #"status" a)
    (#"disabled" (funcall next '()))
    (#"enabled"
      (case (maps:get #"template" a)
        (#"undefined" (funcall next '())) 
        ((= _ tid) 
          (with_availability_template tid (lambda (t)
            (with_template_shifts t next))))))
    (#"edit" 
      (db:join
        (list #"shifts" (maps:get #"id" a))
        #"shift"
        (list #"id" #"template" #"availability" #"day" #"month" #"year" #"start_day" #"start_hour" #"start_min" #"end_day" #"end_hour" #"end_min" #"duration" #"type" )
        #"start_day" 
        next))))


(defun set_availability_property (a prop v next)
  (set_availability_properties a (map prop v) next))

(defun set_availability_properties (a props next)
  (db:hmset (list #"availability" (maps:get #"id" a)) props next))

(defun update_availability_shift (a s y m d sd sh sm ed eh em next)
  (let* ((id (maps:get #"id" s))
         (duration (kit:int2bin (shift_duration sd sh sm ed eh em)))
         (s2 (maps:merge s (map #"year" (kit:int2bin y) #"month" (kit:int2bin m) #"day" (kit:int2bin d) 
                                #"start_day" (kit:int2bin sd) #"start_hour" (kit:int2bin sh) #"start_min" (kit:int2bin sm) 
                                #"end_day" (kit:int2bin ed) #"end_hour" (kit:int2bin eh) #"end_min" (kit:int2bin em) #"duration" duration))))
    (with_availability_shifts a (lambda (shifts)
      (no_overlap? (sort_shifts (replace_shift s2 shifts)) (lambda ()
        (db:hmset (list #"shift" id) s2 (lambda (s3)
          (set_availability_property a #"dirty" #"true" (lambda (_)
            (funcall next s3)))))))))))
  
(defun create_availability_shift (a y m d sd sh sm ed eh em next)
  (create_standalone_availability_shift a #"available" y m d sd sh sm ed eh em #M() 'true (lambda (shift)
    (set_availability_property a #"dirty" #"true" (lambda (_)
      (funcall next shift))))))

(defun copy_availability_shift (a s y m d sd sh sm ed eh em next)
  (create_availability_shift a y m d sd sh sm ed eh em next))

(defun remove_availability_shift (a s next)
  (db:del (list #"shift" (maps:get #"id" s)) (lambda ()
    (db:remove_child a s #"shifts" (lambda ()
      (set_availability_property a #"dirty" #"true" (lambda (_)
        (funcall next))))))))

(defun availability_to_template (u o a name next)
  (create_availability_template u o name (lambda (t)
    (with_availability_shifts u a (lambda (shifts)
      (lists:map (lambda (s) (add_availability_template_shift u t s)) shifts)
      (set_availability_properties a (map #"template" (maps:get #"id" t) #"status" #"enabled" #"dirty" #"false") (lambda (a2)
        (remove_shifts a shifts (lambda ()
          (funcall next a2)))))))))) 

(defun duplicate_template_shift (u s t2 next)
  (let* ((id (kit:uuid))
         (tid (maps:get #"id" t2))
         (oid (maps:get #"organization" t2))
         (owner (maps:get #"id" u))
         (s2 (maps:merge s (map #"id" id #"created" (kit:now) #"template" tid #"organization" oid #"ownedby" owner #"createdby" owner))))
    (db:hmset (list #"shift" id) s2 (lambda (s3)
      (db:add_child t2 s3 #"shifts" (lambda ()
        (funcall next s3)))))))


(defun duplicate_template_shifts
  ((u '() t2 next)(funcall next))
  ((u (cons s rest) t2 next)
    (duplicate_template_shift u s t2 (lambda (_)
      (duplicate_template_shifts u rest t2 next)))))

(defun duplicate_template (u t t2 next)
  (with_template_shifts t (lambda (shifts)
    (duplicate_template_shifts u shifts t2 next)))) 
    
(defun duplicate_schedule_template (u o t name next)
  (create_template u o name (lambda (t2)
    (duplicate_template u t t2 (lambda ()
      (common:with_access u t2 (lambda (t3)
        (funcall next t3))))))))

(defun duplicate_availability_template (u o t name next)
  (create_availability_template u o name (lambda (t2)
    (duplicate_template u t t2 (lambda ()
      (common:with_access u t2 (lambda (t3)
        (funcall next t3))))))))

(defun clear_participant_stats (s next)
  (db:remove_children! s #"participant-stats" #"stats" next))

(defun add_participant_stats (s u stats next)
  (stats:update (maps:put #"id" (kit:uuid) stats) (lambda (stats2)
    (db:add_child s stats2 #"participant-stats" next))))

(defun with_participant_stats (s next)
  (stats:with_stats s #"participant-stats"
    (list #"ownedby" #"s_std_time_w" #"s_idle_time_w" #"s_training_time_w" #"av_time_w" #"s_extra_time_w" ) next)) 
