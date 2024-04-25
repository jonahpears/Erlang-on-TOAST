

%% if mon=role_fsm then module used is "role_fsm_" ++ name (same as ID)
%% if mon=role_gen then only ID is "role_gen_" ++ name
-record(role_modules, { sup = role_sup,
                        mon = role_fsm, %% or role_tmpl
                        imp = role_imp }).

-record(sup_flags, { strategy = one_for_all,
                     intensity = 1,
                     period = 5 }).

-record(role_spec, { name,
                     modules = #role_modules{},
                     params = [] }).


-record(child_options, { restart = transient,
                         shutdown = 2000,
                         type = worker }).

-record(statem_data, { name,
                       coparty_id, 
                       init_state,
                       states = [], 
                       msgs = #{},
                       timeouts = #{}, 
                       state_map = #{},
                       queued_actions = [],
                       options = #{ allow_delayable_sends => false, 
                                    printout_enabled => true,
                                    queue_actions => #{enabled => true, flush_after_recv => true},
                                    forward_receptions => #{ enabled => false, to => undefined}} }).

-record(stop_data, {reason, statem_data = #statem_data{}}).

