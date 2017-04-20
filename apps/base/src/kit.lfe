(defmodule kit
  (export all))

(defun cfg (k v)
  (case (application:get_env 'base k)
    ('undefined 'undefined )
    ((tuple 'ok s)
      (case (proplists:get_value v s)
        ('undefined 'undefined)
        ((= _ s2) 
          (case (proplists:get_value (env) s2)
            ('undefined (proplists:get_value 'default s2))
            ((= _ r) r )))))))

(defun env ()
  (case (os:getenv "SMARTPLAN_ENV" "dev")
    ("prod" 'prod)
    ("beta" 'beta)
    (_ 'dev)
  ))

(defun i18n (k lang)
  (i18n k lang ()))

(defun i18n (k lang p)
  (case (application:get_env 'base 'i18n)
    ('undefined (i18n_undef k lang ))
    ((tuple 'ok v1)
      (case (proplists:get_value k v1)
        ('undefined (i18n_undef k lang ))
        (v2 
          (case (proplists:get_value lang v2)
            ('undefined (i18n_undef k lang))
            (t (kit:fmt t p))))))))

(defun i18n_undef (k lang)
  (kit:fmt "??~s.~s??" (list k lang)))    

(defun timed (action fun)
  (case (cfg 'kit 'timed)
    ('false (funcall fun))
    ('true 
      (let (((tuple t out) (timer:tc fun)))
        (log "[Time] [~p] [~pms]~n" (list action (/ t 1000)))
        out))))

(defun log (fmt params) (io:format fmt params))

(defun log (c fmt params) 
  (case (cfg c 'verbose)
    ('undefined (log fmt params))
    ('true (log fmt params))
    ('false )))

(defun ip2bin (ip) (list_to_binary (inet_parse:ntoa ip)))

(defun atom2bin (a)
  (erlang:atom_to_binary a 'latin1))


(defun jsond (bin)
  (try (jiffy:decode bin '(return_maps))
    (catch ((tuple _ _ _) 'error))))

(defun fmt (t p)
  (erlang:iolist_to_binary 
    (io_lib:format (xmerl_ucs:to_utf8 t) p )))

(defun err (t r)
  (tuple 'error (tuple t (map 'error t 'reason r ))))

(defun uuid ()
  (list_to_binary (uuid:to_string (uuid:uuid4))))

(defun bin2int (b)
  (try (list_to_integer (binary_to_list b))
    (catch ((tuple _ _ _) 'error ))))

(defun bin2bool (b)
  (case b
    (#"true" 'true)
    (_ 'false)))

(defun bool2int (b)
  (case b
    ('true 1)
    ('false 0)))

(defun bin2num (b) 
  (try (list_to_float (binary_to_list b))
    (catch ((tuple _ _ _) (bin2int b)))))

(defun bin2lower (b)
  (unicode:characters_to_binary (string:to_lower (unicode:characters_to_list b))))

(defun int2bin (b) 
  (list_to_binary (integer_to_list b)))

(defun datefield2bin (i)
  (let (((list r)(io_lib:format "~2..0B" (list i))))
    (list_to_binary r)))

(defun float2bin (b dec)
  (list_to_binary (float_to_list b (list (tuple 'decimals dec) 'compact))))

(defun float2bin (b)
  (float2bin b 1))

(defun num2bin (n)
  (num2bin n 1))

(defun num2bin (n dec)
  (case (is_float n)
    ('true (float2bin n dec))
    ('false (int2bin n))))

(defun catbin 
  (('() r) r )
  (((cons h t) r) (catbin t (catbin r h)))
  ((a b) (binary (a binary) (b binary))))

(defun catbin 
  (((cons h t)) (catbin t h)))

(defun joinbin (bins sep)
  (let ((r (lists:foldl 
    (lambda (b acc) (catbin (list acc b sep))) #"" bins)))
      (binary:part r (tuple 0  (- (byte_size r) 1)))))

(defun joinbin (bins)
  (lists:foldl (lambda (b acc) (catbin (list acc b))) #"" bins))

(defun val (s i) 
  (val s i '()))

(defun val
  (((cons h t) i r) 
    (case (valf h i)
      ((= (tuple 'error r) e) e)
      ((= _ v) (val t i (cons v r)))))
  (('() _ r) (tuple 'ok (lists:reverse r))))

(defun lang
  ((#"en") 'en)
  ((#"fr") 'fr)
  ((#"es") 'es)
  ((_) 'en))

(defun valf
  (((tuple 'text f) i) 
    (case (maps:get f i #"")
      ('null (err 'missing f))
      (#"" (err 'missing f))
      ((= _ v) v)))
  (((tuple 'email f) i)
    (case (valf (tuple 'text f) i)
      ((= (tuple 'error _) e) e)
      ((= _ v)
        (case (re:run v #"\\b[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}\\b" )
          ('nomatch (err 'invalid f))
          (_ v )))))
  (((tuple 'lang f) i)
    (lang (maps:get f i)))
  (((tuple 'password f1 f2) i) 
    (case (tuple (valf (tuple 'text f1) i) (valf (tuple 'text f2) i))
      ((tuple (= (tuple 'error _) e) _) e)
      ((tuple _ (= (tuple 'error _) e)) e)
      ((tuple (= _ v1)(= _ v2)) 
        (case (== v1 v2)
          ('false (err 'mismatch f1))
          ('true v1)))))
  (((tuple 'app f) i) 
    (case (valf (tuple 'text f) i)
      ((= (tuple 'error _) e) e)
      ((= _ id)
        (case (kit:cfg 'apps id )
          ('undefined (err 'invalid f))
          ((= _ app) 
            (maps:from_list (cons (tuple 'id id) app)))))))
  (((tuple 'file f) i) (valf (tuple 'text f ) i))
  (((tuple 'int f) i)
    (case (valf (tuple 'text f) i)
      ((= (tuple 'error _) e) e)
      ((= _ v)
        (case (is_integer v)
          ('true v)
          ('false 
          (case (kit:bin2int v)
            ('error (err 'invalid f))
            ((= _ int) int)))))))
  (((tuple 'float f) i)
    (case (valf (tuple 'text f) i)
      ((= (tuple 'error _) e) e)
      ((= _ v)
        (case (is_float v)
          ('true v)
          ('false 
            (case (is_integer v)
              ('true (float v))
              ('false
                (case (kit:bin2num v)
                  ('error (err 'invalid f))
                  ((= _ num) num)))))))))
  (((tuple 'bool f) i)
    (case (valf (tuple 'text f) i)
      ((= (tuple 'error _) e) e)
      ('true 'true)
      (#"true" 'true)
      (_ 'false)))) 
  

(defun now ()
  (let (((tuple m s _) (os:timestamp)))
    (+ s (* m 1000000 ))))

(defun now_micro ()
  (erlang:system_time 'micro_seconds))

(defun days_till
  (((tuple y m d))
    (days_till (calendar:date_to_gregorian_days (tuple y m d))))
  (((= _ days))
    (let (((tuple today _) (calendar:universal_time)))
      (case (- days (calendar:date_to_gregorian_days today))
        (left (when (> left 0)) left)
        (_ 0)))))

(defun days_in
  (((tuple n 'days)) 
    (let (((tuple today _) (calendar:universal_time)))
      (+ n (calendar:date_to_gregorian_days today))))
  (((= _ n))
    (days_in (tuple n 'days))))

;;
;; Adds the specified number of days or 
;; weeks to the given date and returns the 
;; new date
;;
(defun date_add 
  ((from n 'days) 
    (calendar:gregorian_days_to_date
      (+ n (calendar:date_to_gregorian_days from))))
  ((from n 'weeks)
    (date_add from (* 7 n) 'days)))

;; 
;; Returns the number of weeks from today's week
;; till the given week and year. We start
;; at the beginning of the week (monday) and we
;; count seven days per week.
;;
(defun weeks_till (y w)
  (let (((tuple y0 w0) (calendar:iso_week_number )))
    (+ (+ w (- 53 w0)) (* 53 (- (- y 1) y0)))))   

;;
;; Trick to make dow_days_till easier
;; to implement
(defun easy_dow 
  ((0) 7)
  ((dow) dow))

;;
;; Goes back to the monday of the same
;; week of the given date
;;
(defun goto_monday (date) 
  (date_add date
    (* -1 (- (easy_dow (calendar:day_of_the_week date)) 1)) 'days))

;;
;; Goes to the sunday of the same 
;; week of the given date
;;
(defun goto_sunday (date) 
  (date_add date (- 7 (easy_dow (calendar:day_of_the_week date))) 'days))

;; 
;; Goes to the specified week of the year
;; and day of the week
;;
(defun goto_week_day (y w d)
  (date_add (goto_monday (goto_week y w)) (- (easy_dow d) 1 ) 'days))

;;
;; Goes the the specified week of the year
;; and stays in the same day of the week
;; as today's date.
;;
(defun goto_week (y w) 
  (let (((tuple today now) (calendar:local_time)))
    (date_add today (weeks_till y w) 'weeks)))


(defun prefix_each (p all)
  (lists:map (lambda (e)
    (catbin p e)) all ))

(defun new_counters 
  ((m (= (cons (tuple k v) t) c )) 
    (case (maps:is_key k m)
      ('true (new_counters (maps:put k (+ (maps:get k m) v) m) t))
      ('false (new_counters (maps:put k 0 m) c))))
  ((m '()) m)
  ((m c)
    (new_counters m (maps:to_list c))))

(defun mladd (k v m)
  (case (maps:is_key k m)
    ('true (maps:put k (cons v (maps:get k m)) m))
    ('false (maps:put k (list v) m))))

(defun mlrem (k v m)
  (case (maps:is_key k m)
    ('true (maps:put k (lists:delete v (maps:get k m)) m))
    ('false m)))

;;
;; Does a map:get on a nested map
;;
(defun mmget (k1 k2 m ) 
  (maps:get k2 (maps:get k1 m)))

;;
;; Does a map:put on a nested map
;;
(defun mmput (k1 k2 v m) 
  (maps:put k1 m (maps:put k2 v (maps:get k1 m)))) 

;;
;; Does a map:remove on a nested map
;;
(defun mmremove (k1 k2 m)
  (maps:put k1 m (maps:remove k2 (maps:get k1 m))))

;; Converts all float values to binary
;; in the given map
(defun mfloat2bin (m)
  (maps:map (lambda (k v)
    (case (is_float v)
      ('true (float2bin v))
      ('false v))) m))

;;
;; Serializes the given map to a string, using the 
;; two separators
;;
(defun map2string (m sep entry_sep) 
  (maps:fold (match-lambda 
    ((k v #"") (joinbin (list k v) sep))
    ((k v acc) (joinbin (list acc (joinbin (list k v) sep)) entry_sep))) #"" m))


;; Returns the specified integer, if positive
;; or zero
(defun nint (v)
  (case (>= v 0 )
    ('true v)
    ('false 0)))

(defun sign (v)
  (case (== 0 v)
    ('true 0)
    ('false 
      (case (> v 0)
        ('true 1)
        ('false -1)))))

(defun luniq (l)
  (sets:to_list (sets:from_list l)))
