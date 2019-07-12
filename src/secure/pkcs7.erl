%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.


-module (pkcs7).

-export ([pad/1]).
-export ([unpad/1]).

pad(Bin) ->
  Diff = byte_size(Bin) rem 16,
  pad(Bin, 16-Diff).

pad(Bin, 1) ->
  <<Bin/binary,1>>;
pad(Bin, 2) ->
  <<Bin/binary,2,2>>;
pad(Bin, 3) ->
  <<Bin/binary,3,3,3>>;
pad(Bin, 4) ->
  <<Bin/binary,4,4,4,4>>;
pad(Bin, 5) ->
  <<Bin/binary,5,5,5,5,5>>;
pad(Bin, 6) ->
  <<Bin/binary,6,6,6,6,6,6>>;
pad(Bin, 7) ->
  <<Bin/binary,7,7,7,7,7,7,7>>;
pad(Bin, 8) ->
  <<Bin/binary,8,8,8,8,8,8,8,8>>;
pad(Bin, 9) ->
  <<Bin/binary,9,9,9,9,9,9,9,9,9>>;
pad(Bin, 10) ->
  <<Bin/binary,10,10,10,10,10,10,10,10,10,10>>;
pad(Bin, 11) ->
  <<Bin/binary,11,11,11,11,11,11,11,11,11,11,11>>;
pad(Bin, 12) ->
  <<Bin/binary,12,12,12,12,12,12,12,12,12,12,12,12>>;
pad(Bin, 13) ->
  <<Bin/binary,13,13,13,13,13,13,13,13,13,13,13,13,13>>;
pad(Bin, 14) ->
  <<Bin/binary,14,14,14,14,14,14,14,14,14,14,14,14,14,14>>;
pad(Bin, 15) ->
  <<Bin/binary,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15>>;
pad(Bin, 16) ->
  <<Bin/binary,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16>>.

unpad(Bin) ->
  Length = byte_size(Bin),
  Padding = binary:last(Bin),
  DataLength = Length-Padding,
  <<Data:DataLength/binary, _:Padding/binary>> = Bin,
  Data.
