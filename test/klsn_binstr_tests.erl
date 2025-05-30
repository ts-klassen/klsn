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
    
    %% Test encoding all chars
    BinStr = iolist_to_binary(lists:seq(0, 255)),
    Expect = <<
        "%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10",
        "%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F",
        "%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789",
        "%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E"
        "_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~",
        "%7F%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92",
        "%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6",
        "%A7%A8%A9%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA",
        "%BB%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE",
        "%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF%E0%E1%E2",
        "%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1%F2%F3%F4%F5%F6",
        "%F7%F8%F9%FA%FB%FC%FD%FE%FF"
    >>,
    ?assertEqual(Expect, klsn_binstr:urlencode(BinStr)),
    
    ?assertError(badarg, klsn_binstr:urlencode(123)),

    ok.

%% Tests for urldecode/1
urldecode_1_test() ->
    %% Test decoding of an empty string
    ?assertEqual(<<>>, klsn_binstr:urldecode(<<>>)),
    
    %% Test decoding of a string with only safe characters
    ?assertEqual(<<"HelloWorld123-_~.">>, klsn_binstr:urldecode(<<"HelloWorld123-_~.">>)),
    
    %% Test decoding of a string with unsafe characters
    ?assertEqual(<<"Hello World!">>, klsn_binstr:urldecode(<<"Hello%20World%21">>)),
    
    %% Test decoding of a string with mixed safe and unsafe characters
    ?assertEqual(<<"foo/bar?baz=qux">>, klsn_binstr:urldecode(<<"foo%2Fbar%3Fbaz%3Dqux">>)),
    
    %% Test decoding of a string with mixed safe and unsafe characters in lower case
    ?assertEqual(<<"foo/bar?baz=qux">>, klsn_binstr:urldecode(<<"foo%2fbar%3fbaz%3dqux">>)),

    %% Test decoding of non-ASCII characters
    ?assertEqual(<<"こんにちは"/utf8>>, klsn_binstr:urldecode(<<"%E3%81%93%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF">>)),
    
    %% Test decoding of special characters
    ?assertEqual(<<" \t\r\n">>, klsn_binstr:urldecode(<<"%20%09%0D%0A">>)),
    
    %% Test decoding all chars
    Expect = iolist_to_binary(lists:seq(0, 255)),
    BinStr = <<
        "%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10",
        "%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F",
        "%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789",
        "%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E"
        "_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~",
        "%7F%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92",
        "%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6",
        "%A7%A8%A9%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA",
        "%BB%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE",
        "%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF%E0%E1%E2",
        "%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1%F2%F3%F4%F5%F6",
        "%F7%F8%F9%FA%FB%FC%FD%FE%FF"
    >>,
    ?assertEqual(Expect, klsn_binstr:urldecode(BinStr)),

    ?assertError(badarg, klsn_binstr:urldecode(123)),
    
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

%% Additional replace/2 edge-cases
replace_edge_cases_test() ->
    %% Overlapping pattern
    ?assertEqual(<<"aa">>, klsn_binstr:replace([{<<"aa">>, <<"a">>}], <<"aaaa">>)),

    %% Replacing a pattern with itself (no-op)
    ?assertEqual(<<"foofoo">>, klsn_binstr:replace([{<<"foo">>, <<"foo">>}], <<"foofoo">>)),

    %% Very large binary
    N = 100000,
    LargeA = binary:copy(<<"a">>, N),
    LargeB = binary:copy(<<"b">>, N),
    ?assertEqual(LargeB, klsn_binstr:replace([{<<"a">>, <<"b">>}], LargeA)),

    ok.
