-module(role_imp_).
-file("role_imp_.erl",1).

-include_lib("stdlib/include/assert.hrl").

-include("tpri_data_records.hrl").

-export([ start_link/0,
          init/1 ]).

-export([ start_link/1 ]).

-define(NAME, put_name_here).

-include("printout.hrl").
-include("role_cb_funs.hrl").

start_link() -> ?MODULE:start_link([]).

start_link(Params) -> 
  printout(?NAME, "~p.", [?FUNCTION_NAME]),
  Params1 = maps:from_list(Params),
  RoleID = maps:get(reg_id,Params1),
  ?assert(RoleID==?MODULE, io_lib:format("Error in ~p: Module of file (~p) does not match Role/ID provided (~p).",[?MODULE,?MODULE,RoleID])),
  Name = maps:get(name,Params1),
  ?assert(Name==?NAME, io_lib:format("Error in ~p: Name in file (~p) does not match Name provided (~p).",[?MODULE,?NAME,Name])),
  PID = erlang:spawn_link(?MODULE, init, [Params]),
  printout(?NAME, "leaving ~p as : ~p.", [?FUNCTION_NAME, PID]),
  {ok, PID}.


%% @doc init sequence -- waits to receive PID of monitor
init(Params) ->
  printout(?NAME, "~p.", [?FUNCTION_NAME]),

  Params1 = maps:from_list(Params),
  Name = maps:get(name,Params1),
  ?assert(Name==?NAME, io_lib:format("Error in ~p: Name in file (~p) does not match Name provided (~p).",[?MODULE,?NAME,Name])),

  %% get app ID and send self()
  [{app_id,AppID}|_T] = ets:lookup(tpri,app_id),
  AppID ! {tpri, Name, imp, self()},

  %% wait to receive coparty ID
  receive
    {setup_coparty, CoPartyID} -> 
      receive
        {setup_finished, start} -> main(CoPartyID)
      end
  end.


main(_CoPartyID) -> ok.


