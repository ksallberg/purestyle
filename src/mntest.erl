-module(mntest).

-record(employee, {emp_no,
                   name,
                   salary}).

%% store usernames
-record(user, {username,
               password}).

%% link username <-> listname
%% to keep track of all lists
-record(user_lists, {username,
                     listname}).

%% Keep all URL'associated with a list
-record(list, {listname,
               url}).

-export([test/0, test2/0, test3/0]).

test() ->
    NodeList = [node()],
    mnesia:create_schema(NodeList),
    mnesia:start(),
    mnesia:create_table(employee,
                        [{attributes, record_info(fields, employee)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(user,
                        [{attributes, record_info(fields, user)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(user_lists,
                        [{attributes, record_info(fields, user_lists)},
                         {disc_copies, NodeList}]),
    mnesia:create_table(list,
                        [{attributes, record_info(fields, list)},
                         {disc_copies, NodeList}]).

test2() ->
    Emp1 = #employee{emp_no=12, name="kristian", salary=100},
    Emp2 = #employee{emp_no=20, name="naitsirk", salary=10},
    insert_emp(Emp1),
    insert_emp(Emp2).

test3() ->
    AllEmps = all_employees(),
    io:format("~p~n", [AllEmps]).

all_employees() ->
    F = fun() ->
            mnesia:select(employee, [{#employee{name   = '$1',
                                                salary = '$2',
                                                _      = '_'},
                                      [],
                                      ['$1']
                                    }])
        end,
    mnesia:transaction(F).

insert_emp(Emp) ->
    Fun = fun() ->
              mnesia:write(Emp)
          end,
    mnesia:transaction(Fun).
