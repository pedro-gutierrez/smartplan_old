(defmodule www
  (export (start 1) (init 3) (terminate 3) (handle 2)))

(defun start (mod) 
  (cowboy:start_http 'http (kit:cfg 'http 'acceptors) `(#(port ,(kit:cfg 'http 'port))) `(#(env (#(dispatch 
    ,(cowboy_router:compile `(#(_( 
      #( "/api/:action" www ,mod )
      #( "/" cowboy_static #(priv_file ,mod "static/index.html"))
      #( "/[...]" cowboy_static #(priv_dir ,mod "static" ))
    ))))))))))

(defun init (any req state) (tuple 'ok req state))

(defun terminate (reason req state) 'ok)

(defun reply (req state code body )
  (reply req state code body '()))

(defun reply (req state code body headers)
  (log "[Out] ~p~n" (list body))
  (let* ((headers2 (cons #("content-type" "application/json") headers))
        ((tuple 'ok req2) (cowboy_req:reply code headers2 (jiffy:encode body) req ))) 
    (tuple 'ok req2 state)))

(defun handle (req state)
  (let (((tuple a req2) (cowboy_req:binding 'action req)))
    (kit:timed a (lambda() 
      (with-body req2 state (lambda (req3 body)
        (let* (
          ((tuple (tuple ip _) req4) (cowboy_req:peer req3))
          ((tuple qs req5) (cowboy_req:qs_vals req4))
          ((tuple ua req6) (cowboy_req:header #"user-agent" req5 #""))
          ((tuple lang req7) (cowboy_req:header #"lang" req6 #""))
          ((tuple session req8) (cowboy_req:header #"session" req7 #""))
          ((tuple app req9) (cowboy_req:header #"app" req8 #""))
          (input (maps:merge 
            (maps:merge (maps:from_list qs) body) 
            (map #"action" a #"ip" (kit:ip2bin ip) 
              #"ua" ua #"lang" lang #"session" session #"app" app)))
          (req10 (cowboy_req:compact req9)))
            (case (route input state)
              ((tuple 'error (tuple 'not_found r))(reply req10 state 404 r))
              ((tuple 'error (tuple 'invalid r))(reply req10 state 400 r))
              ((tuple 'error (tuple 'missing r))(reply req10 state 400 r))
              ((tuple 'error (tuple 'forbidden r))(reply req10 state 401 r))
              ((tuple 'error (tuple 'not_implemented r))(reply req10 state 501 r))
              ((tuple 'error (tuple 'conflict r))(reply req10 state 409 r))
              ((tuple 'error (tuple _ r))(reply req10 state 500 r))
              ((tuple 'file f)(send-file req10 state f))
              (json (reply req10 state 200 json))))))))))

(defun with-body (req state next)
  (case (cowboy_req:has_body req)
    ('false (funcall next req #M()))
    ('true 
      (case (cowboy_req:parse_header #"content-type" req) 
        ((tuple 'ok (tuple #"multipart" #"form-data" _) req2)(with-file req2 state next))
        ((tuple 'ok (tuple #"application" #"json" _) req2)(with-json req2 state next))
        ((tuple 'ok _ req2) (reply req2 state 400 #M(reason content_type)))))))

(defun with-file (req state next)
  (let (((tuple 'ok part_headers req2)(cowboy_req:part req)))
    (case (cowboy_req:part_body req2 (list (tuple 'length (kit:cfg 'http 'upload_limit))(tuple 'read_length 128000)))
      ((tuple 'more _ req3)(reply req3 state 400 #M(reason file_too_big)))
      ((tuple 'ok data req3)
        (let ((size (byte_size data))
              ((tuple 'file #"file" name content_type _)(cow_multipart:form_data part_headers)))
            (let* ((id (kit:uuid))
                  (path (binary_to_list (kit:catbin (list (kit:cfg 'http 'upload_dir) #"/" id)))))
              (case (file:write_file path data)
                ((tuple 'error e)(reply req3 state 500 (map 'error 'cant_write_file 'reason path)))
                ('ok (funcall next req3 (map #"file" (map #"id" id #"size" size #"type" content_type #"name" name #"created" (kit:now))))))))))))

(defun send-file (req state file)
  (let ((size (mref file #"size" ))
        (name (mref file #"name" ))
        (path (kit:catbin (list (kit:cfg 'http 'upload_dir) #"/" (mref file #"id")))))
    (case (file:read_file_info path)
      ((tuple 'error _)
        (case (maps:is_key 'fallback file)
          ('false (reply req state 404 (map 'error 'not_found 'reason path)))
          ('true (send-file req state (maps:put #"id" (maps:get 'fallback file) file)))))
      ((tuple 'ok _)
        (let* ((content_type (mref file #"type"))
              (content_length (filelib:file_size path))
              (send_fun (lambda(s t)(call t 'sendfile s path)))
              (req2 (cowboy_req:set_resp_body_fun content_length send_fun req))
              ((tuple 'ok req3)(cowboy_req:reply 200 (list
                (tuple #"Content-Type" content_type )
                (tuple #"Content-disposition" (kit:catbin #"attachment; filename=" name) )
                (tuple #"Content-Type" #"public, max-age=1" )
                (tuple #"Access-Control-Allow-Origin" #"*" )) req2)))
          (tuple 'ok req3 state))))))  
  
(defun with-json (req state next)
  (let (((tuple 'ok body req2) (cowboy_req:body req)))
      (case (kit:jsond body)
        ('error (reply req2 state 400 #M(reason bad_json)))
        ((= _ json)(funcall next req2 json)))))

(defun route (i mod)
  (log "[In] ~p~n" (list i))
  (let ((a (map-get i #"action")))
    (case (call mod 'do a) 
      ((tuple 'error e) (tuple 'error e))
      ((tuple acl v f)
        (case (security a acl i)
          ((tuple 'error e)(tuple 'error e))
          ((tuple 'ok app user) 
            (case (kit:val v i )
              ((tuple 'error e)(tuple 'error e))
              ((tuple 'ok p)(funcall f app user p)))))))))

(defun security (a acl i)
  (case (kit:val '(#(app #"app") #(lang #"lang") #(text #"ua") #(text #"ip")) i)
    ((tuple 'error e) (tuple 'error e))
    ((tuple 'ok (list app lang ua ip))
      (let ((root (kit:cfg 'security 'root))
            (token (map-get i #"session")))
        (case token
          (#"" 
            (case acl
              ('anonymous (tuple 'ok app (map 'lang lang)))
              (_ (kit:err 'forbidden a))))
          (_ (when (== root token))
            (case acl
              ('root (tuple 'ok app (map 'lang lang)))
              (_ (kit:err 'forbidden a))))
          (_ 
            (case acl
              ('anonymous (kit:err 'forbibden a))
              ('root (kit:err 'forbidden a))
              (_ 
                (case (users:find_session token app ua ip)
                  ((tuple 'error _) (kit:err 'forbidden 'invalid_session))
                  ((= _ user)
                    (users:extend_session token (lambda ()
                      (case acl  
                        ('signed_in (tuple 'ok app user))
                        (_ 
                          (case (users:get_valid_acl user (kit:atom2bin acl) )
                            ((tuple 'error _) (kit:err 'forbidden 'invalid_access))
                            (_ (tuple 'ok app user)))))))))))))))))

(defun log (fmt p)
  (case (kit:cfg 'http 'verbose)
    ('true (kit:log fmt p))
    ('false 'ok )))
