% %% HELP URL:http://wiki.mg.open.qq.com/index.php?title=Android%E6%94%AF%E4%BB%98API#.E6.9F.A5.E8.AF.A2.E4.BD.99.E9.A2.9D.E6.8E.A5.E5.8F.A3

-module(qq_android_server).

% -behaviour(poolboy_worker).
% -behaviour(gen_server).

% %% API
% -export([start_link/1]).

% %% gen_server callbacks
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%     terminate/2, code_change/3]).

% -export([verify_receipt/4]).

% -define(DEBUG_IPS, [{115,29,14,9},
%                     {192,168,2,16},
%                     {192,168,2,14}]).

% -define(RESEND_RECEIPT_MILLISECONDS, 15000).
% -define(HTTP_CLIENT_TIMEOUT, 10000).

% %% 查询余额接口
% %% 【沙箱】http://msdktest.qq.com/mpay/get_balance_m
% %% 【现网】http://msdk.qq.com/mpay/get_balance_m 
% %% 扣除游戏币接口
% %% 【沙箱】http://msdktest.qq.com/mpay/pay_m
% %% 【现网】http://msdk.qq.com/mpay/pay_m
% %% 取消支付接口
% %% 【现网】http://msdk.qq.com/mpay/cancel_pay_m
% %% 【沙箱】http://msdktest.qq.com/mpay/cancel_pay_m
% %% 直接赠送接口
% %% 【现网】http://msdk.qq.com/mpay/present_m
% %% 【沙箱】http://msdktest.qq.com/mpay/present_m
% %% 预转账接口
% %% 【沙箱】http://msdktest.qq.com/mpay/pre_transfer
% %% 【现网】http://msdk.qq.com/mpay/pre_transfer
% %% 转账确认接口
% %% 【沙箱】http://msdktest.qq.com/mpay/confirm_transfer
% %% 【现网】http://msdk.qq.com/mpay/confirm_transfer
% %% 转账回滚接口
% %% 【沙箱】http://msdktest.qq.com/mpay/cancel_transfer
% %% 【现网】http://msdk.qq.com/mpay/cancel_transfer
% -define(QQ_ANDROID_TEST_VERIFY_URL, "http://msdktest.qq.com/mpay/get_balance_m").
% -define(QQ_ANDROID_VERIFY_URL, "http://msdk.qq.com/mpay/get_balance_m").
% -define(QQ_ANDROID_URL, <<"/mpay/get_balance_m">>).

% -define(TAB, ?MODULE).

% -record(state, {}).

% %%%===================================================================
% %%% API
% %%%===================================================================
% start_link(Args) ->
%     gen_server:start_link(?MODULE, Args, []).

% verify_receipt(Id, Product, Pay, Callback) ->
%     poolboy:transaction(qq_android_verify_pool, fun(Worker) ->
%         gen_server:cast(Worker, {verify_receipt, Id, Product, Pay, Callback})
%     end).

% %%%===================================================================
% %%% gen_server callbacks
% %%%===================================================================

% init([]) ->
%     {ok, #state{}}.

% handle_call(_Request, _From, State) ->
%     Reply = ok,
%     {reply, Reply, State}.

% %% 发校验
% handle_cast({verify_receipt, Id, Product, Pay, Callback}, State) ->
%     case ets:lookup(?TAB, {verify_receipt, Id}) of
%         [] ->
%             do_verify_receipt(Id, Product, Pay, Callback);
%         _ -> ok
%     end,
%     {noreply, State}.

% handle_info({ibrowse_async_headers, _ReqId, _Code, _Headers}, State) ->
%     {noreply, State};
% handle_info({ibrowse_async_response, ReqId, Response}, State) ->
%     [{_, Id}] = ets:lookup(?TAB, {req_id, ReqId}),
%     [{_, {ProductId, Amount, Callback}}] = ets:lookup(?TAB, {verify_receipt, Id}),
%     ets:delete(?TAB, {req_id, ReqId}),
%     ets:delete(?TAB, {verify_receipt, Id}),
%     handle_pay_info(Id, ProductId, Amount, Callback, Response),
%     {noreply, State};
% handle_info({ibrowse_async_response_end, _ReqId}, State) ->
%     {noreply, State};
% handle_info(Msg, State) ->
%     error_logger:info_msg("handle_info Msg: ~p~n", [Msg]),
%     {noreply, State}.

% terminate(_Reason, _State) ->
%     ok.

% code_change(_OldVsn, State, _Extra) ->
%     {ok, State}.

% %%%===================================================================
% %%% Internal functions
% %%%===================================================================

% do_verify_receipt(Id, {ProductId, Amount, AccType}, {OpenId, OpenKey, PayToken, Pf, PfKey}, Callback) ->
%     Url = get_url(),
%     Params = pack_params(OpenId, OpenKey, PayToken, Pf, PfKey, AccType),
%     {ibrowse_req_id, ReqId} = http:async_request(Url, get, Params),
%     ets:insert(?TAB, {{verify_receipt, Id}, {ProductId, Amount, Callback}}),
%     ets:insert(?TAB, {{req_id, ReqId}, Id}).

% % 正确返回示例
% % Content-type: text/html; charset=utf-8
% % {"ret":0,"balance":200,"gen_balance":0, "first_save":1,”save_amt”:200}
% % 错误返回示例
% % Content-type: text/html; charset=utf-8
% % {"ret":1018,"msg":"请先登录"}
% handle_pay_info(Id, ProductId, Amount, Callback, Response) ->
%     ReceiptDataList = re:replace(Response, "[\n\t ]", "", [global,caseless,{return, list}]),
%     JsonObject = jsx:decode(list_to_binary(ReceiptDataList)),
%     error_logger:info_msg("JsonObject: ~p~n", [JsonObject]),
%     case lists:keyfind(<<"ret">>, 1, JsonObject) of
%         {<<"ret">>, 0} -> 
%             case lists:keyfind(<<"balance">>, 1, JsonObject) of
%                {<<"balance">>, Balance} when Balance =:= Amount ->
%                     Callback(ProductId);
%                false -> 
%                     error_logger:info_msg("QQ Android Response: balance: ~p~n", [JsonObject]),
%                     not_status     
%             end;
%         {<<"ret">>, Ret} ->
%             error_logger:info_msg("QQ Android Response: ret: ~p~n", [JsonObject]),
%             not_status;
%         false ->
%             error_logger:info_msg("QQ Android Response: false: ~p~n", [JsonObject]),
%             not_status     
%     end.

% get_url() ->
%     IP = os_utils:get_ip(),
%     case lists:member(IP, ?DEBUG_IPS) of
%         true -> ?QQ_ANDROID_TEST_VERIFY_URL;
%         false -> ?QQ_ANDROID_VERIFY_URL
%     end.

% get_appid(AccType) ->
%     case AccType of 
%         <<"wx_account">> -> <<"wx0493e8db3cb904ca">>;
%         <<"qq_account">> -> <<"1104821006">>
%     end.

% get_appkey(AccType) ->
%     case AccType of 
%         <<"wx_account">> -> <<"cac406d1f35c5db694af3fdfb6972017&">>;
%         <<"qq_account">> -> <<"pJFcxj4xOYImq5oF&">>
%     end.

% conn_string(Key, Value) ->
%     binary_string:join([Key, Value], <<"=">>).

% conn_integer(Key, Value) ->
%     binary_string:join([Key, integer_to_binary(Value)], <<"=">>).

% pack_params(OpenId, OpenKey, PayToken, Pf, PfKey, AccType) ->
%     AppId = get_appid(AccType),
%     Now = time_utils:now(),
%     List = [<<"GET">>,
%             ?QQ_ANDROID_URL,
%             conn_string(<<"appid">>, AppId), 
%             conn_string(<<"openid">>, OpenId),
%             conn_string(<<"openkey">>, OpenKey),
%             conn_string(<<"pay_token">>, PayToken),
%             conn_string(<<"pf">>, Pf),
%             conn_string(<<"pfkey">>, PfKey),
%             conn_integer(<<"ts">>, Now),
%             conn_integer(<<"zoneid">>, 1)],
%     Bin = binary_string:join(List, <<"&">>),
%     Data = list_to_binary(http_uri:encode(binary_to_list(Bin))),
%     Key = get_appkey(AccType),
%     Sig = base64:encode(crypto:hmac(sha, Key, Data)),
%     [{<<"appid">>, AppId},
%      {<<"openid">>, OpenId},
%      {<<"openkey">>, OpenKey},
%      {<<"pay_token">>, PayToken},
%      {<<"pf">>, Pf},
%      {<<"pfkey">>, PfKey},
%      {<<"sig">>, Sig},
%      {<<"ts">>, Now},
%      {<<"zoneid">>, 1}].