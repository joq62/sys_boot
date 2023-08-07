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
 
-export([start/1]).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec])->
   
    ok=setup(ClusterSpec),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(2000),
 %   init:stop(),
    ok.



loop()->
    io:format("Start ~p~n",[{time(),?MODULE,?FUNCTION_NAME}]),
%     io:format("sd all ~p~n",[sd:all()]),
%    EtcdNodes=sd:get_node(etcd),
 %   SystemInfo=[{N,rpc:call(N,mnesia,system_info,[],5000)}||N<-EtcdNodes],
  %  io:format("SystemInfo~p~n",[SystemInfo]),
   
    timer:sleep(30*1000),
    
    loop().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=sys_boot:start(ClusterSpec),
    pong=sys_boot:ping(),

    ok.
