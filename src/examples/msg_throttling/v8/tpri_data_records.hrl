

%% if mon=role_fsm then module used is "role_fsm_" ++ name (same as ID)
%% if mon=role_gen then only ID is "role_gen_" ++ name
-record(role_modules, { sup = role_sup,
                        mon = role_fsm, %% or role_tmp
                        imp = role_imp }).

% -record(sup_flags, { strategy = one_for_all,
%                      intensity = 1,
%                      period = 5 }).

-record(role_spec, { name,
                     modules = #role_modules{},
                     params = [] }).


-record(child_options, { restart = transient,
                         shutdown = 2000,
                         type = worker }).

% -record(statem_data, { role, name,
%                        coparty_id, 
%                        init_state,
%                        states = [], 
%                        msgs = #{},
%                        timeouts = #{}, 
%                        state_map = #{},
%                        queued_actions = [],
%                        options = #{ delayable_sends => #{enabled => false}, 
%                                     printout => #{enabled => true, verbose => false},
%                                     queue_actions => #{enabled => true, flush_after_recv => #{ any => true, spec_labels => []}},
%                                     forward_receptions => #{ enabled => false, to => undefined, any => true, spec_labels => []}} }).

% -record(stop_data, {reason, statem_data = #statem_data{}}).

