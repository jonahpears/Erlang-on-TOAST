-module(gen_monitor).
% -behaviour(gen_statem).

-export([ start_link/0,
          start_link/1,
          init/1
          ]).

start_link() -> start_link([]).

start_link(Args) when is_list(Args) -> 
  Data = Args,%mon_data(Args),
  #{name := Name} = Data,
  {ok, PID} = gen_statem:start_link({global, Name}, ?MODULE, [Data], []),
  {ok, PID}.

init([Data]) when is_map(Data) ->
  
  {ok, init_setup_state, Data}.
