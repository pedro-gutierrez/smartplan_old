-module(users).
-export([set/4, with_user/2, with_user/3, create/5, create_ptoken/2, find_by_email/2, resolve/1, find_by_id/2, find_by_acl/2, on_valid_ptoken/3, set_passwd/3, on_valid_passwd/3, create_session/5, delete_session/2, find_session/5, find_session/4, make_public/1, set_name/4, set_name/3, on_read_access/3, set_avatar/2, create_acls/2, get_acls/2,get_acls/1, get_acl/2, get_valid_acl/2, notify_signup/4, notify_passwd_changed/3, notify_passwd_forgot/4, extend_session/2]).


create( First, Last, Email, Lang, N) ->
	db:setnx([<<"email">>, Email], fun(Id) ->
		db:hmset([ <<"user">>, Id], #{
			first => First,
			last => Last,
			lang => Lang,
			email => Email,
			id => Id,
                        photo => kit:cfg( assets, avatar ) 
		}, fun(U) ->
			app:incr( <<"emails">>, 1, fun(_) ->
			  app:incr( <<"users">>, 1, fun(_) -> N(U) end)		
			end)			  
		end)			  
	end).

create_ptoken(U, N) ->
	UserId = maps:get( <<"id">>, U),
	T = kit:uuid(),
	db:hmset([ <<"ptoken">>, UserId], #{
		email => maps:get( <<"email">>, U),
		token => T
	}, fun(_) -> 
		db:expire([ <<"ptoken">>, UserId], 7200, fun() -> N(T) end)	
	end).

resolve(Id)-> 
  db:hgetall([<<"user">>, Id]).

find_by_id( Id, N) -> 
  db:hgetall([<<"user">>, Id], N).

with_user(Uid, N) -> find_by_id(Uid, N).

with_user( U, Uid, N ) ->
  find_by_id( Uid, fun(User) ->
    common:with_user_access( U, User, fun(User2) ->
      N(User2)
    end)
  end).

find_by_acl( A, N) ->
  db:join_alpha(
    [<<"users">>, A],
    <<"user">>,
    [<<"id">>, <<"first">>, <<"last">>, <<"email">>, <<"photo">>, <<"lang">>, <<"issue_emails">> ],
    <<"last">>,
    N).

find_by_email( Email, N) ->
	case db:get([<<"email">>, Email] ) of
		{error, {not_found, _} } -> kit:err(not_found, no_such_account);
		{error, E } -> {error, E};
		Id -> find_by_id( Id, N)
	end.
		
on_valid_ptoken(U, T, N) ->
	UserId = maps:get(<<"id">>, U),
	case db:hgetall([ <<"ptoken">>, UserId]) of
		{error, {not_found, _}} -> kit:err( not_found, invalid_token);
		{error, E} -> {error, E};
		M ->
			UserEmail = maps:get(<<"email">>, U),
			StoredToken = maps:get( <<"token">>, M ),
			StoredEmail = maps:get( <<"email">>, M ),
			case kit:catbin( T, UserEmail ) =:= kit:catbin( StoredToken, StoredEmail ) of
				true -> N();
				false -> kit:err(not_found, token)
			end
	end.
	
set_passwd(U, P, N) ->
	Id = maps:get( <<"id">>, U),
	db:hmset([ <<"user">>, Id], #{password => crypt(P)}, fun(_) ->
		db:del([<<"ptoken">>, Id], N)																	 
	end).


on_valid_passwd( U, Passwd, N ) ->
    case maps:is_key( <<"password">>, U) of
      false -> kit:err( not_found, invalid_credentials );
      true ->
        Hash = maps:get(<<"password">>, U),
        case verify( Passwd, Hash) of
          false -> kit:err( not_found, invalid_credentials );
          true -> N()
      end
    end.

create_session(U, App, Ua, Ip, N) ->
    T = kit:uuid(),
    UserId = maps:get( <<"id">>, U),
    AppId = maps:get( id, App ),
    db:hmset([<<"session">>, T], #{ user => UserId, ua => Ua, ip => Ip, app => AppId }, fun(_) ->
        db:expire([ <<"session">>, T], 7200, fun() -> N(T) end)                    
    end).

extend_session( T, Next) ->
  db:expire([<<"session">>, T], 7200, Next).

delete_session( T, N ) -> db:del([<<"session">>, T], N).
    

find_session(T, App, Ua, Ip, N) ->
    db:hgetall([<<"session">>, T], fun(S) ->
        AppId = maps:get(id, App),
        SessionUa = maps:get( <<"ua">>, S),
        SessionIp = maps:get( <<"ip">>, S),
        SessionApp = maps:get( <<"app">>, S),
        case kit:catbin([Ua, Ip, AppId]) =:= kit:catbin([SessionUa, SessionIp, SessionApp ]) of
            false -> kit:err( forbidden, session_invalid );
            true ->
                SessionUser = maps:get( <<"user">>, S),
                db:hgetall([<<"user">>, SessionUser], fun(U) -> N(make_public(U)) end )
        end                                    
    end).

find_session( T, App, Ua, Ip ) -> find_session( T, App, Ua, Ip, fun(S) -> S end).

make_public(U) -> maps:remove( <<"password">>, U).

set_name( U, F, L, N) ->
    Id = maps:get( <<"id">>, U),
    db:hmset([<<"user">>, Id ], #{ first => F, last => L}, N).

set_name( U, F, L ) -> 
    set_name(U, F, L, fun(P) -> make_public(P) end).


access(U, P ) ->
    UId = maps:get(<<"id">>, U),
    PId = maps:get(<<"id">>, P),
    case UId =:= PId of
        true -> write;
        false -> read
    end.

on_read_access( U, P, N) ->
    case access(U, P) of
        none -> kit:err( forbidden, read);
        _ -> N()
    end.

set_avatar(U, Photo) ->
	Id = maps:get(<<"id">>, U),
	db:hmset([<<"user">>, Id], #{ photo => Photo}, fun(P) ->
		make_public(P)			  
	end).

create_acls(U, N) ->
  Acls = kit:cfg( security, acls ),
  Exp = kit:cfg( security, expiration ),
  create_acls(U, Acls, Exp, N).

create_acls(_, [], _, N) -> N();

create_acls(U, [A|R], Exp, N) ->
  create_acl(U, A, Exp, fun() ->
    create_acls(U, R, Exp, N) 
  end).

create_acl(U, A, Exp, N) ->
  Id=maps:get(<<"id">>, U),
  db:hmset([<<"acl">>, Id, A], #{expires => kit:days_in(Exp), name =>A}, fun(_) ->
    db:sadd([<<"acls">>, Id], [A], fun(_) ->
      db:sadd([ <<"users">>, A ], [Id], fun(_) -> N() end)
    end)
  end).

get_acls(U, N) ->
	Id=maps:get(<<"id">>, U),
	db:sort_alpha([<<"acls">>, Id], fun(S) ->
		N(lists:map( fun(A) -> get_acl(U, A) end, S))										   
	end).

get_acls(U) -> get_acls(U, fun(A) -> A end).


get_acl(U, Name) ->
	Id=maps:get(<<"id">>, U),
	db:hgetall([<<"acl">>, Id, Name], fun(A) ->
		Exp = maps:get(<<"expires">>, A),									   
		maps:put( left, kit:days_till( kit:bin2int( Exp)), A )			  
	end).
	

get_valid_acl( U, Name ) ->
    Acl = get_acl( U, Name ),
    case maps:get( <<"expires">>, Acl ) of
      0 -> kit:err( 'forbidden', 'acl_expired' );
      _ -> Acl
    end.

notify_signup(U, App, Lang, T ) -> 
	To = maps:get( <<"email">>, U),
	First = maps:get( <<"first">>, U),
	mail:send( To, kit:i18n( signup_subject, Lang), [
		kit:i18n( hello, Lang, [First]),
		kit:i18n( signup_body, Lang, [app:url(App, reset_password, T )]),
		kit:i18n( footer, Lang)
	]).
	
notify_passwd_changed(U, App, Lang) -> 
	To = maps:get( <<"email">>, U),
	First = maps:get( <<"first">>, U),
	mail:send( To, kit:i18n( reset_password_subject, Lang), [
		kit:i18n( hello, Lang, [First]),
		kit:i18n( reset_password_body, Lang, [app:url(App, sign_in )]),
		kit:i18n( footer, Lang)
	]).	
	
notify_passwd_forgot(U, App, Lang, T ) -> 
	To = maps:get( <<"email">>, U),
	First = maps:get( <<"first">>, U),
	mail:send( To, kit:i18n( forgot_password_subject, Lang), [
		kit:i18n( hello, Lang, [First]),
		kit:i18n( forgot_password_body, Lang, [app:url(App, reset_password, T )]),
		kit:i18n( footer, Lang)
	]).	

crypt(P) ->
  {ok, Key} = pbkdf2:pbkdf2(sha, P, <<"salt">>, 500),
  pbkdf2:to_hex(Key).

verify(P, H) -> 
  pbkdf2:compare_secure(crypt(P), H).

set(U, P, V, N) ->
  db:hmset( [<<"user">>, maps:get( <<"id">>, U)], #{ P => V }, N ).
