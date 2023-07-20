%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_sys_boot).

%% API
-export([
	 init_monitor_nodes/0,
	 store_deployments/1,
	 deploy/1,
	 is_deployed/1,
	 get_node/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init_monitor_nodes()->
    AllDeploymentsId=db_deploy:get_all_id(),
    init_monitor_nodes(AllDeploymentsId).
  
init_monitor_nodes([Id|T])->
    {ok,Node}=db_deploy:read(node,Id),
    erlang:monitor_node(Node,true),
    init_monitor_nodes(T).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

store_deployments(DeploymentSpec)->
    {ok,Deployment}=db_deployment_spec:read(deployment,DeploymentSpec),
    [vm_appl_control:create_deployment(DeploymentSpec,ProviderSpec,HostSpec)||{ProviderSpec,HostSpec}<-Deployment].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deploy(DeploymentId)->
    vm_appl_control:start_deployment(DeploymentId).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_deployed(_DeploymentId)->
    

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_node(DeploymentId)->
    db_deploy:read(node,DeploymentId).

    
%%%===================================================================
%%% Internal functions
%%%===================================================================
