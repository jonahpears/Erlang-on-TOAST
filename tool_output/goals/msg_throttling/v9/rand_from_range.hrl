-compile({nowarn_unused_function, [ {rand_from_range,2} ]}).

%% https://stackoverflow.com/a/67385550
rand_from_range(Lowerbound, Upperbound) -> rand:uniform(Upperbound - Lowerbound + 1) + Lowerbound - 1.
