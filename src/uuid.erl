% Copyright (c) 2012, Heroku <nem@erlang.geek.nz>
% Rewritten to use crypto instead of random
%
% Original Source, API:
% Copyright (c) 2008, Travis Vachon
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
%     * Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%
%     * Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in the
%       documentation and/or other materials provided with the distribution.
%
%     * Neither the name of the author nor the names of its contributors
%       may be used to endorse or promote products derived from this
%       software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
-module(uuid).
-export([v4/0, to_string/1, to_binary/1, to_iolist/1]).
-export([binary_uuid_type/1
        ]).

-ifdef(TEST).
-opaque binary_uuid() :: <<_:16>>.
-type binary_string_uuid() :: <<_:36>>.
-else.
-opaque binary_uuid() :: <<_:128>>.
-type binary_string_uuid() :: <<_:288>>.
-endif.

-type tuple_uuid() :: {TL::non_neg_integer(),
                       TM::non_neg_integer(),
                       THV::non_neg_integer(),
                       CSR::non_neg_integer(),
                       CSL::non_neg_integer(),
                       N::non_neg_integer()}.

-type string_uuid() :: string().
-type iolist_uuid() :: iolist().

-export_type([binary_string_uuid/0
              ,binary_uuid/0
             ]).

% Generates a random binary UUID.
-spec v4() -> binary_uuid().
v4() ->
    <<R1:48,R2:12,R3:32,R4:30,_:6>> = crypto:rand_bytes(16),
    v4(R1,R2,R3,R4).

-spec v4(R1::non_neg_integer(), R2::non_neg_integer(),
         R3::non_neg_integer(), R4::non_neg_integer()) ->
                binary_uuid().
v4(R1, R2, R3, R4)
  when is_integer(R1), is_integer(R2),
       is_integer(R3), is_integer(R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>.

-spec to_string(binary_uuid()) -> string_uuid().
% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(to_iolist(U)).

-spec to_binary(binary_uuid()) -> binary_string_uuid().
to_binary(U) ->
    iolist_to_binary(to_iolist(U)).

-spec to_iolist(binary_uuid()) -> iolist_uuid().
to_iolist(U) ->
    io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                  tuple_to_list(get_parts(U))).

-spec binary_uuid_type(binary()) -> binary_uuid | binary_string_uuid | invalid.
binary_uuid_type(<<_TL:32, _TM:16, _THV:16, _CSR:8, _CSL:8, _N:48>>) ->
    binary_uuid;
%% "a532a9e0-5263-42d8-8a32-87dccc3b1cd0"
binary_uuid_type(<<_TL:8/binary, $-,
                   _TM:4/binary, $-,
                   _THV:4/binary, $-,
                   _CSR:2/binary, _CSL:2/binary,
                   _N:12/binary>>) ->
    binary_string_uuid;
binary_uuid_type(_) ->
    invalid.

-spec get_parts(binary_uuid()) -> tuple_uuid().
% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    {TL, TM, THV, CSR, CSL, N}.
