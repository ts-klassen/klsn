-module(klsn_binstr_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for urlencode/1
urlencode_1_test() ->
    %% Test encoding of an empty string
    ?assertEqual(<<>>, klsn_binstr:urlencode(<<>>)),
    
    %% Test encoding of a string with only safe characters
    ?assertEqual(<<"HelloWorld123-_~">>, klsn_binstr:urlencode(<<"HelloWorld123-_~">>)),
    
    %% Test encoding of a string with unsafe characters
    ?assertEqual(<<"Hello%20World%21">>, klsn_binstr:urlencode(<<"Hello World!">>)),
    
    %% Test encoding of a string with mixed safe and unsafe characters
    ?assertEqual(<<"foo%2Fbar%3Fbaz%3Dqux">>, klsn_binstr:urlencode(<<"foo/bar?baz=qux">>)),
    
    %% Test encoding of non-ASCII characters
    ?assertEqual(<<"%E3%81%93%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF">>, klsn_binstr:urlencode(<<"こんにちは">>)),
    
    %% Test encoding of special characters
    ?assertEqual(<<"%25%26%2B%2C%2F%3A%3B%3D%3F%40">>, klsn_binstr:urlencode(<<"%&+,/:;=?@">>)),
    
    %% Test encoding of hexadecimal boundary characters
    ?assertEqual(<<"%00%FF">>, klsn_binstr:urlencode(<<0, 255>>)),
    
    ok.
