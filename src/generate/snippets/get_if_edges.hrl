
%% empty case
get_if_edges([]) -> {{undefined,undefined}, {undefined,undefined}};

%% if timer 
get_if_edges([#edge{to=To,is_if=true,is_else=false,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=false}}}=H|T]) ->
  %% get tail, which must go second
  {{undefined,undefined}, Tail} = get_if_edges(T),
  %% return first
  {{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}},Tail};
%%
%% if timer else
get_if_edges([#edge{to=To,is_if=false,is_else=true,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=false}}}=H|T]) ->
  %% get tail, which must go first 
  {Tail, {undefined,undefined}} = get_if_edges(T),
  %% return second
  {Tail,{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}}};
%%
%% if not timer 
get_if_edges([#edge{to=To,is_if=true,is_else=false,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=true}}}=H|T]) ->
  %% get tail, which must go first 
  {Tail, {undefined,undefined}} = get_if_edges(T),
  %% return second
  {Tail,{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}}};
%%
%% if not timer else
get_if_edges([#edge{to=To,is_if=false,is_else=true,edge_data=#edge_data{if_stmt=#{is_timer:=IsTimer,ref:=Ref,is_not:=true}}}=H|T]) ->
  %% get tail, which must go second
  {{undefined,undefined}, Tail} = get_if_edges(T),
  %% return first
  {{H,#{to=>To,is_timer=>IsTimer,ref=>Ref}},Tail}.
%%