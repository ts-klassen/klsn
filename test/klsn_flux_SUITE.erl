-module(klsn_flux_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Export the suite callback and test cases
-export([all/0,
         main/1]).

%% Define the test suite with all/0
all() ->
    [
        main
    ].

%% Just make sure there are no errors

main(_Config) ->
    Org = default,
    Bucket = sandbox,
    Measurement = klsn_db:new_id(),
    Point = #{
        measurement => Measurement,
        field => #{
            test => 1
        }
    },
    ok = klsn_flux:write(Org, Bucket, Point),
    ok.

