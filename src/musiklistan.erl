-module(musiklistan).
-export([reg_user/2,
         check_login/2
        ]).

-define(l2b, list_to_binary).
-define(b2l, binary_to_list).

-record(usercookie, {username, times=0}).

reg_user(Username, Password) ->
    Bucket = <<"users">>,
    case mldb:get_v(Bucket, ?l2b(Username)) of
        no_such_key ->
            mldb:put_kv(Bucket, ?l2b(Username), ?l2b(Password)),
            user_registered;
        _ ->
            user_already_existing
    end.

check_login(Username, Password) ->
    Bucket = <<"users">>,
    case mldb:get_v(Bucket, ?l2b(Username)) of
        no_such_key -> login_fail;
        DBPassword  ->
            case ?b2l(DBPassword) == Password of
                false -> login_fail;
                true  ->
                    CookieRecord = #usercookie{username=Username},
                    Cookie       = yaws_api:new_cookie_session(CookieRecord),
                    C = yaws_api:set_cookie("usersession",
                                            Cookie,
                                            [{path, "/"}]),
                    {login_ok, C}
            end
    end.
