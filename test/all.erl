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
    ok=delete_provider_dirs("c50"),
  
  %  ok=loop(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
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
delete_provider_dirs(HostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    io:format("HostSpec ~p~n",[{HostSpec,?MODULE,?FUNCTION_NAME}]),
 %   HostSpec="c50",
    {ok,Ip}=sd:call(etcd,etcd_host,get_ip,[HostSpec],5000),
    {ok,Port}=sd:call(etcd,etcd_host,get_port,[HostSpec],5000),
    {ok,Uid}=sd:call(etcd,etcd_host,get_user,[HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,etcd_host,get_passwd,[HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="pwd",
    {ok,[HomeDir]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    io:format("HomeDir ~p~n",[{HomeDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,Files}=ssh_server:send_msg(Ip,Port,Uid,Pwd,"ls ",TimeOut),
    io:format("Files ~p~n",[{Files,?MODULE,?FUNCTION_NAME,?LINE}]),
%    {ok,Files}=file:list_dir(HomeDir),
    ProviderDirs=[filename:join(HomeDir,File)||File<-Files,
					       ".provider_dir"==filename:extension(File)],
    
    DelDir=[{ssh_server:send_msg(Ip,Port,Uid,Pwd,"rm -r "++File,TimeOut),File}||File<-ProviderDirs],
  %  DelDir=[{file:del_dir_r(File),File}||File<-ProviderDirs],
    io:format("DelDir ~p~n",[{DelDir,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,Files2}=ssh_server:send_msg(Ip,Port,Uid,Pwd,"ls ",TimeOut),
    []=[filename:join(HomeDir,File)||File<-Files2,
					    ".provider_dir"==filename:extension(File)],
   
    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

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
