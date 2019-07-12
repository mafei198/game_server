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


-module(secure).

-export([encrypt/3,
		 decrypt/3]).

%% Key: 32 byte, IVec: 16 byte
-spec(encrypt(Key::binary()|iolist(), IVec::binary(), Text::binary()) -> Cipher::binary()).
encrypt(Key, IVec, Text) ->
    PadedText = pkcs7:pad(Text),
    crypto:block_encrypt(aes_cbc256, Key, IVec, PadedText).

-spec(decrypt(Key::binary()|iolist(), IVec::binary(), Cipher::binary()) -> Text::binary()).
decrypt(Key, IVec, Cipher) ->
    Text = crypto:block_decrypt(aes_cbc256, Key, IVec, Cipher),
    pkcs7:unpad(Text).
