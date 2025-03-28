-module(klsn_binstr).

-export([
        urlencode/1
      , from_any/1
      , replace/2
      , hash/1
    ]).

-export_type([
        binstr/0
    ]).

-type binstr() :: unicode:unicode_binary().


-spec hash(binstr()) -> binstr().
hash(Binary) ->
    list_to_binary(
        string:lowercase(
            binary_to_list(
                binary:encode_hex(
                    crypto:hash(sha256, Binary)
                )
            )
        )
    ).

-spec replace([{binstr(), binstr()}], binstr()) -> binstr().
replace(Rule, Sub) ->
    lists:foldl(fun({Before, After}, Acc) ->
        binary:replace(Acc, Before, After, [global])
    end, Sub, Rule).

-spec from_any(any()) -> binstr().
from_any(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
from_any(Float) when is_float(Float) ->
    float_to_binary(Float);
from_any(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
from_any(IOList) ->
    iolist_to_binary(IOList).

-spec urlencode(binstr()) -> binstr().
urlencode(Bin) ->
    IsSafe = fun
        ($-) -> true;
        ($.) -> true;
        ($_) -> true;
        ($~) -> true;
        (C) when $A =< C, C =< $Z -> true;
        (C) when $a =< C, C =< $z -> true;
        (C) when $0 =< C, C =< $9 -> true;
        (_) -> false
    end,
    urlencode(Bin, IsSafe, <<>>).

urlencode(<<>>, _, Acc) ->
    Acc;
urlencode(<<Char/utf8, Rest/binary>>, IsSafe, Acc) ->
    Encoded = case IsSafe(Char) of
        false ->
            percent_encode(<<Char/utf8>>, <<>>);
        true ->
            <<Char/utf8>>
    end,
    urlencode(Rest, IsSafe, <<Acc/binary, Encoded/binary>>);
urlencode(_, _, _) ->
    error(badarg).

percent_encode(<<>>, Acc) ->
    Acc;
percent_encode(<<A:4, B:4, Rest/binary>>, Acc) ->
    EncodedLeft = encode_half_char(A),
    EncodedRight = encode_half_char(B),
    percent_encode(Rest, <<
        Acc/binary
      , $%
      , EncodedLeft
      , EncodedRight
    >>).

encode_half_char(HalfChar) ->
    if
        ((HalfChar) >= 0) andalso ((HalfChar) =< 9) -> (HalfChar) + $0;
        ((HalfChar) >= 10) andalso ((HalfChar) =< 15) -> (HalfChar) + $A - 10
    end.



