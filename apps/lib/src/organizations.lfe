(defmodule organizations
  (export all))

(defun create (u name next)
  (common:on_new_name u #"organizations" name (lambda()
    (let ((owner (: maps get #"id" u))
          (id (kit:uuid)))
      (db:hmset (list #"organization" id) 
        (map #"id" id #"name" name #"createdby" owner #"created" (kit:now) #"ownedby" owner #"status" #"active") 
        (lambda (o)
          (common:add_name u #"organizations" name (lambda()
            (add_member o u (lambda(_)
              (index o (lambda () (funcall next o)))))))))))))

(defun index (done)
  (with_organizations (lambda (orgs)
    (each orgs done (lambda (o next)
      (index o next))))))

(defun index (o next)
  (db:sadd #"organizations" (list (maps:get #"id" o)) (lambda (_)
    (funcall next))))

(defun with_organizations (next)
  (db:join_alpha 
    (list #"organizations")
    #"organization"
    (list #"name" #"id" #"status" #"created" #"ownedby" #"createdby")
    #"name"
    next))

(defun each
  (('() finish each) (funcall finish))
  (((cons o rest) finish each)
    (funcall each o (lambda () (each rest finish each)))))

(defun with_organizations (u next)
  (db:join_alpha 
    (list #"organizations" (: maps get #"id" u))
    #"organization"
    (list #"name" #"id" #"status" #"created" #"ownedby" #"createdby")
    #"name"
    (lambda (orgs) 
      (funcall next (lists:map (lambda (o) (common:with_org_access u o (lambda (o2) o2))) orgs)))))
  
(defun with_access ( u o )
  (common:with_org_access u o (lambda (o2) o2)))

(defun with_organizations (u p next)
  (with_organizations p (lambda (orgs)
    (funcall next (lists:map (lambda (o) (with_access u o)) orgs)))))

(defun with_organization (u id next)
  (with_organization id (lambda (o) 
    (common:with_org_access u o (lambda (o2) 
      (funcall next o2 ))))))

(defun with_organization (id next)
  (db:hgetall (list #"organization" id) next)) 

(defun with_organization_or_empty (id next)
  (case (db:hgetall (list #"organization" id))
    ((tuple 'error _) (funcall next #M()))
    ((= _ o) (funcall next o))))

(defun add_member (o u next)
  (db:add_child u o #"organizations" (lambda ()
    (db:add_child o u #"members" (lambda ()
      (common:add_contrib u o (lambda (_)
        (notifications:organization u #"membership_created" o (lambda ()
          (funcall next o))))))))))

(defun remove_member (o u next)
  (db:remove_child o u #"members" (lambda ()
    (db:remove_child u o #"organizations" (lambda ()
      (common:remove_contrib u o (lambda ()
        (notifications:organization u #"membership_removed" o (lambda () 
          (funcall next))))))))))

(defun with_members (o next)
  (let* ((users (db:join_alpha
                  (list #"members" (maps:get #"id" o))
                  #"user"
                  (list #"id" #"first" #"last" #"email" #"photo" )
                  #"last" 
                  (lambda (users) users)))
         (members (lists:map (lambda (u) (with_member o u (lambda (m) m))) users)))
    (funcall next members)))

(defun with_member (o u next)
  (common:with_contrib u o (lambda (contrib)
    (funcall next (maps:merge contrib u)))))

(defun set_owner (u o old_owner new_owner next)
  (let ((old_owner_id (maps:get #"id" old_owner))
        (new_owner_id (maps:get #"id" new_owner))
        (org_name (maps:get #"name" o)))
  (case (== old_owner_id new_owner_id)
    ('true (funcall next o))
    ('false 
      (common:on_new_name new_owner #"organizations" org_name (lambda () 
        (db:hmset 
          (list #"organization" (maps:get #"id" o)) 
          (map #"ownedby" new_owner_id)
          (lambda (o2)
            (common:remove_name old_owner #"organizations" org_name (lambda ()
              (common:add_name new_owner #"organizations" org_name (lambda ()
                (common:with_access u o2 (lambda (o3) 
                  (funcall next o3)))))))))))))))

(defun rename (u o name next)
  (let ((old_name (maps:get #"name" o)))
    (case (== name old_name) 
      ('true (funcall next o))
      ('false
        (common:on_new_name u #"organizations" name (lambda()
          (db:hmset (list #"organization" (maps:get #"id" o)) (map #"name" name) 
            (lambda (o2)
              (common:add_name u #"organizations" name (lambda()
              (common:remove_name u #"organizations" old_name (lambda () 
                (common:with_access u o2 (lambda (o3) o3))))))))))))))

(defun add_tag (o tag next)
 (common:add_tag o tag next)) 

(defun remove_tag (o tag next)
 (common:remove_tag o tag next)) 

(defun with_tags (o next)
 (common:get_tags o next)) 

(defun with_tag (o tag next )
 (common:with_tag o tag next))

(defun add_member_tag (o m tag next)
  (common:add_tag m o tag next)) 
    
(defun remove_member_tag (o m tag next)
  (common:remove_tag m o tag next ))

(defun get_member_tags (o m next)
  (common:get_tags m o next))

(defun set_member_profile (o u p next)
  (common:with_contrib u o (lambda (c)
    (db:hmset (list #"contrib" (maps:get #"id" c))
      (map #"profile" (maps:get #"id" p)) (lambda (_)
        (with_member o u (lambda (m)
          (funcall next (users:make_public m)))))))))
