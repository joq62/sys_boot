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
	 init_etcd/1,
	 init_control/0,

	 stop_nodes/1,
	 delete_providers/1,
	 init_monitor_nodes/0,
	 store_deployments/1,
	
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
init_etcd(ClusterSpec)->
    ok=application:start(etcd),
    Lock=list_to_atom(ClusterSpec),
    ok=etcd_paas_config:create(ClusterSpec,Lock),
    ok=etcd_lock:create(Lock),
    {ok,DeploymentRecords}=etcd_deployment_record:create_records(ClusterSpec),
    ok=etcd_cluster:set_deployment_records(DeploymentRecords,ClusterSpec),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init_control()->
    ok=application:start(control),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

stop_nodes(ClusterSpec)->
    {ok,DeploymentRecords}=etcd_cluster:get_deployment_records(ClusterSpec),
    stop_nodes(DeploymentRecords,[]).
    
stop_nodes([],Acc)->    
    Acc;
stop_nodes([DeploymentRecord|T],Acc)->
    R=lib_control_node:stop_node(DeploymentRecord),
    stop_nodes(T,[R|Acc]).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_providers(ClusterSpec)->
    {ok,DeploymentRecords}=etcd_cluster:get_deployment_records(ClusterSpec),
    delete_providers(DeploymentRecords,[]).
    
    
delete_providers([],Acc)->
    Acc;
delete_providers([DeploymentRecord|T],Acc)->
    {ok,Node}=etcd_deployment_record:get_node(DeploymentRecord),
    {ok,Dir}=etcd_deployment_record:get_dir(DeploymentRecord),
    {ok,HostSpec}=etcd_deployment_record:get_host(DeploymentRecord),
    
    %% Stop Node
    rpc:call(Node,init,stop,[],5000),
    
    {ok,Ip}=sd:call(etcd,etcd_host,get_ip,[HostSpec],5000),
    {ok,Port}=sd:call(etcd,etcd_host,get_port,[HostSpec],5000),
    {ok,Uid}=sd:call(etcd,etcd_host,get_user,[HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,etcd_host,get_passwd,[HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="pwd",
    {ok,[HomeDir]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    ProviderDir=filename:join(HomeDir,Dir),
    DelDir=ssh_server:send_msg(Ip,Port,Uid,Pwd,"rm -r "++ProviderDir,TimeOut),
    NewAcc=case DelDir of
	       {ok,[]}->
		   [{ok,ProviderDir}|Acc];
	       Error->
		   [Error|Acc]
	   end,
    delete_providers(T,NewAcc).

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
