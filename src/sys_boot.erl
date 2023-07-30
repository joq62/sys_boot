%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(sys_boot).
 
-behaviour(gen_server).
 

-include("log.api").
%% API
-export([
	 get_node/1,
	 deploy/1,
	 is_deployed/1,
	 store_deployments/1,
	 ping/0]).


-export([
	 start/1,
	 start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {cluster_spec}).

%%%===================================================================
%%% API
%%%===================================================================
start(ClusterSpec)->
    application:set_env([{sys_boot,[{cluster_spec,ClusterSpec}]}]),
    application:start(sys_boot).
    

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
store_deployments(DeploymentSpec)->
    gen_server:call(?SERVER, {store_deployments,DeploymentSpec},infinity).    
    
deploy(DeploymentId)->
    gen_server:call(?SERVER, {deploy,DeploymentId},infinity).    
    


get_node(DeploymentId)->
    gen_server:call(?SERVER, {get_node,DeploymentId},infinity).    

is_deployed(DeploymentId)->
    gen_server:call(?SERVER, {is_deployed,DeploymentId},infinity).    
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    ok=application:start(log),
    ok=application:start(etcd),
    pong=log:ping(),
    pong=ssh_server:ping(),
    
    {ok,ClusterSpec}=application:get_env(cluster_spec),
    %% 
    ok=etcd_cluster_to_deploy:create(ClusterSpec,node()),
    %% Create Wanted state
DeploymentRecords=
    
    
    {ok,CookieStr}=etcd_cluster:get_cookie_str(ClusterSpec),
    [ClusterSpec]=etcd_cluster:all_clusters(),
    {ok,ClusterSpec}=etcd_cluster_to_deploy:get_cluster_spec(),
    
    
    {ok, #state{cluster_spec=ClusterSpec}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({store_deployments,DeploymentSpec}, _From, State) ->
    Reply=lib_sys_boot:store_deployments(DeploymentSpec),
    {reply, Reply, State};

handle_call({get_node,DeploymentId}, _From, State) ->
    Reply=lib_sys_boot:get_node(DeploymentId),
    {reply, Reply, State};

handle_call({deploy,DeploymentId}, _From, State) ->
    Reply=lib_sys_boot:deploy(DeploymentId),
    {reply, Reply, State};

handle_call({is_deployed,DeploymentId}, _From, State) ->
    Reply=lib_sys_boot:is_deployed(DeploymentId),
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
