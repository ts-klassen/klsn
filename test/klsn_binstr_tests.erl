-module(klsn_binstr_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for urlencode/1
urlencode_1_test() ->
    %% Test encoding of an empty string
    ?assertEqual(<<>>, klsn_binstr:urlencode(<<>>)),
    
    %% Test encoding of a string with only safe characters
    ?assertEqual(<<"HelloWorld123-_~.">>, klsn_binstr:urlencode(<<"HelloWorld123-_~.">>)),
    
    %% Test encoding of a string with unsafe characters
    ?assertEqual(<<"Hello%20World%21">>, klsn_binstr:urlencode(<<"Hello World!">>)),
    
    %% Test encoding of a string with mixed safe and unsafe characters
    ?assertEqual(<<"foo%2Fbar%3Fbaz%3Dqux">>, klsn_binstr:urlencode(<<"foo/bar?baz=qux">>)),
    
    %% Test encoding of non-ASCII characters
    ?assertEqual(<<"%E3%81%93%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF">>, klsn_binstr:urlencode(<<"こんにちは"/utf8>>)),
    
    %% Test encoding of special characters
    ?assertEqual(<<"%20%09%0D%0A">>, klsn_binstr:urlencode(<<" \t\r\n">>)),
    
    %% Test encoding of chars from 32 to 126
    BinStr = iolist_to_binary(lists:seq(32, 126)),
    Expect = <<
        "%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789",
        "%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E"
        "_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~"
    >>,
    ?assertEqual(Expect, klsn_binstr:urlencode(BinStr)),
    
    ok.

%% Tests for hash/1
hash_1_test() ->
    ?assertEqual(
        <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>
      , klsn_binstr:hash(<<>>)
    ),
    ?assertEqual(
        <<"64ec88ca00b268e5ba1a35678a1b5316d212f4f366b2477232534a8aeca37f3c">>
      , klsn_binstr:hash(<<"Hello world">>)
    ),
    ?assertEqual(
        <<"40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880">>
      , klsn_binstr:hash(iolist_to_binary(lists:seq(0, 255)))
    ),
    ok.

%% Tests for from_any/1
from_any_1_test() ->
    ?assertEqual(<<"42">>, klsn_binstr:from_any(42)),

    FloatExpect = erlang:float_to_binary(3.14),
    ?assertEqual(FloatExpect, klsn_binstr:from_any(3.14)),

    ?assertEqual(<<"hello">>, klsn_binstr:from_any(hello)),

    ?assertEqual(<<"foobar">>, klsn_binstr:from_any([<<"foo">>, <<"bar">>])),

    ok.

%% Tests for replace/2
replace_2_test() ->
    ?assertEqual(<<"barbar">>, klsn_binstr:replace([{<<"foo">>, <<"bar">>}], <<"foofoo">>)),

    ?assertEqual(<<"baz baz">>, klsn_binstr:replace([
        {<<"foo">>, <<"bar">>} ,
        {<<"bar">>, <<"baz">>}
    ], <<"foo bar">>)),

    ?assertEqual(<<"abc">>, klsn_binstr:replace([{<<"x">>, <<"y">>}], <<"abc">>)),

    ?assertEqual(<<>>, klsn_binstr:replace([{<<"a">>, <<>>}], <<"a">>)),

    ok.

%% Error cases
urlencode_error_test() ->
    ?assertError(badarg, klsn_binstr:urlencode(123)).
