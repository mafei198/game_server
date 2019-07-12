-module(http).

-behaviour(gen_server).

-define(JSON_CONTENT, {"Content-Type", "application/json"}).
-define(HTTP_CLIENT_TIMEOUT, 10000).
%% Connection pool size: 100, Each connection queue size: 100
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 100}]).

-define(SERVER, ?MODULE).
-define(RESEND_AFTER_DELAY, 20000). % 20 secs
-define(MAX_RETRY, 13).
 
%% API
-export([start_link/0,
         request/3,
         async_request/3,
         queue_request/3,
         queue_request/4]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {}).
 
request(Url, Method, Params) ->
    request(Url, Method, Params, false).

async_request(Url, Method, Params) ->
    request(Url, Method, Params, true).

request(Url, Method, Params, IsAsync) ->
    Options = case IsAsync of
                  true -> ?HTTP_CLIENT_OPTIONS ++ [{stream_to, self()}];
                  false -> ?HTTP_CLIENT_OPTIONS
              end,
    JsonString = case Params of
                     [] -> Params;
                     _  -> jsx:encode(Params)
                 end,
    ibrowse:send_req(Url, [?JSON_CONTENT], Method, JsonString, Options, ?HTTP_CLIENT_TIMEOUT).   
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

queue_request(Url, Method, Params) ->
    gen_server:cast(?SERVER, {queue_request, Url, Method, Params, undefined}).

queue_request(Url, Method, Params, CallBack) ->
    gen_server:cast(?SERVER, {queue_request, Url, Method, Params, CallBack}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-record(request, {uuid, url, method, params, retry}).

init([]) ->
    erlang:send_after(?RESEND_AFTER_DELAY, self(), resend_all_requests),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({queue_request, Url, Method, Params, CallBack}, State) ->
    Uuid = uuid_factory:gen(),
    Request = #request{uuid = Uuid,
                       url = Url,
                       method = Method,
                       params = Params,
                       retry = 0},
    shared_data:write(Request),
    if 
        CallBack =:= undefined -> ok;
        true -> game_counter:set({request, Uuid}, CallBack)
    end, 
    do_send_request(Request),
    {noreply, State}.

handle_info(resend_all_requests, State) ->
    ets:foldl(fun(Request, _) ->
        do_send_request(Request)
    end, unused, request),
    {noreply, State};
handle_info({resend_requst, Uuid}, State) ->
    case shared_data:find(request, Uuid) of
        undefined -> ok;
        Request -> do_send_request(Request)
    end,
    {noreply, State};
handle_info({ibrowse_async_headers, ReqId, Code, _Headers}, State) ->
    %% error_logger:info_msg("handle_info:Response:~p, ~p~n", [ReqId, Code]),
    put({code, ReqId}, Code),
    {noreply, State};
handle_info({ibrowse_async_response, ReqId, Response}, State) ->
    Code = get({code, ReqId}),
    Uuid = get({req_id, ReqId}),
    %% error_logger:info_msg("handle_info:Response: ~p~n", [Response]),
    if
        Code =:= "200" ->
            case game_counter:get({request, Uuid}) of 
                undefined -> ok;
                CallBack -> 
                    CallBack(Response),
                    game_counter:del({request, Uuid})
            end,
            shared_data:find(request, Uuid),
            erase({req_id, ReqId}),
            erase({code, ReqId});
        true ->
            case shared_data:find(request, Uuid) of
                undefined -> ok;
                Request -> 
                    NewRequest = Request#request{retry = Request#request.retry + 1},
                    Retry = NewRequest#request.retry,
                    if
                        Retry > ?MAX_RETRY -> 
                            shared_data:delete(request, Uuid);
                        true ->
                            shared_data:write(NewRequest),
                            Delay = math:pow(2, Retry) * ?RESEND_AFTER_DELAY,
                            logger:info("Http Resend Dealy: ~p, Uuid: ~p~n", [Delay, Uuid]),
                            erlang:send_after(trunc(Delay), self(), {resend_requst, Uuid})
                    end
            end
    end,
    {noreply, State};
handle_info({ibrowse_async_response_end, _ReqId}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    error_logger:info_msg("handle_info Msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_send_request(Request) ->
    %% error_logger:info_msg("do_send_request: ~p", [Request]),
    {ibrowse_req_id, _ReqId} = async_request(Request#request.url, 
                                            Request#request.method, 
                                            Request#request.params),
    %% 临时处理
    shared_data:delete(request, Request#request.uuid).
    %% put({req_id, ReqId}, Request#request.uuid).
    %% 临时处理
    
