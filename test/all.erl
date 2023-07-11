%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).

-define(TestDeploy,"test").

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=delete_provider_dirs(),
    ok=etcd_test(),

    ok=loop(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
    ok.



loop()->
    io:format("Start ~p~n",[{time(),?MODULE,?FUNCTION_NAME}]),
    io:format("sd all ~p~n",[sd:all()]),
    timer:sleep(12*1000),
    
    loop().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_provider_dirs()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    HostSpec="c50",
    {ok,Ip}=sd:call(etcd,db_host_spec,read,[local_ip,HostSpec],5000),
    {ok,Port}=sd:call(etcd,db_host_spec,read,[ssh_port,HostSpec],5000),
    {ok,Uid}=sd:call(etcd,db_host_spec,read,[uid,HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,db_host_spec,read,[passwd,HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="pwd",
    {ok,[HomeDir]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    {ok,Files}=file:list_dir(HomeDir),
    ProviderDirs=[filename:join(HomeDir,File)||File<-Files,
					       ".provider_dir"==filename:extension(File)],
    DelDir=[{file:del_dir_r(File),File}||File<-ProviderDirs],
    io:format("DelDir ~p~n",[{DelDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,Files2}=file:list_dir(HomeDir),
    []=[filename:join(HomeDir,File)||File<-Files2,
					    ".provider_dir"==filename:extension(File)],
   
    

    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
etcd_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    DeploymentSpecs=sys_boot:store_deployments(?TestDeploy),
    %% Find etcd

    EtcdDeploymentIds=[DeploySpec||{ok,DeploySpec}<-DeploymentSpecs,
				     {ok,"etcd"}==db_deploy:read(provider_spec,DeploySpec)],
	       
    io:format("EtcdDeploymentIds ~p~n",[EtcdDeploymentIds]),
    [EtcdId1|_]=EtcdDeploymentIds,
    io:format("EtcdSpec1 ~p~n",[{EtcdId1,?MODULE,?LINE,?FUNCTION_NAME}]),

    %% Deploy Etcd 1
    
    {ok,EtcdId1}=sys_boot:deploy(EtcdId1),
    {ok,NodeEtcd1}=sys_boot:get_node(EtcdId1),
    %% Ensure dbase is loaded to Etcd1
    io:format("~p~n",[{NodeEtcd1,rpc:call(NodeEtcd1,mnesia,system_info,[],5000)}]),


    %% Deploy control 

    ControlDeploymentIds=[DeploySpec||{ok,DeploySpec}<-DeploymentSpecs,
				   {ok,"control"}==db_deploy:read(provider_spec,DeploySpec)],
    
    io:format("ControlDeploymentIds ~p~n",[{ControlDeploymentIds,?MODULE,?LINE,?FUNCTION_NAME}]),
    [ControlId1|_]=ControlDeploymentIds,
    io:format("ControlId1 ~p~n",[{ControlId1,?MODULE,?LINE,?FUNCTION_NAME}]),

    %% Deploy Etcd 1
    
    {ok,ControlId1}=sys_boot:deploy(ControlId1),
    {ok,NodeControlId1}=sys_boot:get_node(ControlId1),
    %% Ensure dbase is loaded to Etcd1
    io:format("~p~n",[{NodeControlId1,rpc:call(NodeControlId1,control,ping,[],5000)}]),
    %% There you go!
    
    io:format("sd all ~p~n",[sd:all()]),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=application:start(log),
    pong=log:ping(),
    ok=application:start(sys_boot),
    pong=sys_boot:ping(),
    pong=ssh_server:ping(),
    pong=etcd:ping(),
    ok.
