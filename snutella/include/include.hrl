%% The state of the peer proces. 
-record(pstate, {name, data=[], peers=[], history=[]}).

%% Information about a peer kept by the host cache. 
-record(peer, {pid, name, items}).


%% Macros used to enrich the logging information. 

-define(INFO(MSG), error_logger:info_report([{?FILE, "line " ++ integer_to_list(?LINE)}] ++ MSG)).
-define(WARNING(MSG), error_logger:warning_report([{?FILE, "line " ++ integer_to_list(?LINE)}] ++ MSG)).

