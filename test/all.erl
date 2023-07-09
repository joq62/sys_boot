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
    
    ok=etcd_test(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
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

    EtcdDeploymentSpecs=[DeploySpec||{ok,DeploySpec}<-DeploymentSpecs,
				     {ok,"etcd"}==db_deploy:read(provider_spec,DeploySpec)],
	       
    io:format("EtcdDeploymentSpecs ~p~n",[EtcdDeploymentSpecs]),
    [EtcdSpec1|_]=EtcdDeploymentSpecs,
    io:format("EtcdSpec1 ~p~n",[EtcdSpec1]),

    %% Deploy Etcd 1

    %% Ensure dbase is loaded to Etcd1


    %% Deploy control 

    %% There you go!
    
    
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=application:start(sys_boot),
    pong=sys_boot:ping(),
    pong=ssh_server:ping(),
    pong=etcd:ping(),
    ok.
