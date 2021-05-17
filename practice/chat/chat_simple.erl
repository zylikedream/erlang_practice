-module(chat_simple).
%-export([start/0,initialize_ets/0,info_lookup/1,loop/2,info_update/3,init/1,handle_call/3,terminate/2]).
%-import(counter,[start/1,add/1,value/1,decrease/1,log_add/1,chat_add/1]).
%-import(my_fsm,[start_count/0,count/1]).
-compile(export_all).
-include("user_info.hrl").
-define(SERVER,?MODULE).

start() ->
    gen_server:start_link({local,?SERVER},?MODULE,[],[]).

init([]) ->
    initialize_ets(),
    start_parallel_server(), 
    {ok,ets:new(mysocket,[public,named_table])}.





%开启服务器
start_parallel_server() ->
    {ok,Listen} = gen_tcp:listen(2345,[binary,{packet,0},{reuseaddr,true},{active,true}]),
    spawn(fun() -> per_connect(Listen) end).

%每次绑定一个当前Socket后再分裂一个新的服务端进程，再接收新的请求
per_connect(Listen) ->
    {ok,Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> per_connect(Listen) end),
    loop(Socket).




%初始化ets
initialize_ets() ->
    ets:new(test,[set,public,named_table,{keypos,#user.name}]),
    ets:insert(test,#user{id=01,name="carlos",passwd="123",login_times=0,chat_times=0,last_login={},state=0}),
    ets:insert(test,#user{id=02,name="qiqi",passwd="123",login_times=0,chat_times=0,last_login={},state=0}),
    ets:insert(test,#user{id=03,name="cym",passwd="123",login_times=0,chat_times=0,last_login={},state=0}).

%查询ets
info_lookup(Key) ->
    %返回值是一个元组
    ets:lookup(test,Key).

%修改ets信息
info_update(Key,Pos,Update) ->
    ets:update_element(test,Key,{Pos,Update}).
 


%<<-------------------------回调函数----------------------------->>
%登录的时候添加socket
handle_call({addSocket,UserName,Socket},_From,Tab) ->
     Reply =   case ets:lookup(Tab,UserName) of
         	 [{UserName,Socket}] -> have_socket;

         	 []  ->  ets:insert(Tab,{UserName,Socket})
         end,  %这里是顺序结构，所以用逗号
         io:format("Tab ~p~n",[Tab]),
        % io:format("mysocket ~p~n",[ets:i(Tab)]),
     {reply,Reply,Tab};

%退出的时候，删除socket
handle_call({deleteSocket,UserName,Socket},_From,Tab) ->
     Reply =   case ets:lookup(Tab,UserName) of
         	 [{UserName,Socket}] -> ets:delete(Tab,UserName);

         	 []  ->   io:format("no exist this  socket ~p~n",[Socket])
         end,  %这里是顺序结构，所以用逗号
        
        % io:format("mysocket ~p~n",[ets:i(Tab)]),
     {reply,Reply,Tab};

%用户在线个数
handle_call({userNumber},_From,Tab) ->
 		 Socketlist = ets:tab2list(Tab),
         io:format("~p user online~n",[length(Socketlist)]),
     {reply,[],Tab};

%广播信息
handle_call({sendAllMessage,Name,Msg},_From,Tab) ->
           %Socketlist = [{UserName,Socket,Name,Msg}||{UserName,Socket} <- ets:tab2list(Tab)],  不用推导了
           Socketlist =[{UserName,Socket}||{UserName,Socket} <- ets:tab2list(Tab),UserName =/= Name],
           io:format("list ~p~n",[Socketlist]),

           lists:foreach(
           	   fun({UserName,Socket}) ->
                    N = term_to_binary(Name), %用户名字长度  竟然可以用外面的变量
        			M = term_to_binary(Msg),	 %信息长度
           			Packet = <<0010:16,(byte_size(N)):16,N/binary,(byte_size(M)):16,M/binary>>, 			
 		     		gen_tcp:send(Socket,Packet)
 		     	end,
 		     	Socketlist
           	),
         	
     {reply,[],Tab}.


%<<-------------------------回调函数----------------------------->>


%用户在线人数。
userNumber() ->
 	  gen_server:call(server_example,{userNumber}).




%接收信息并处理
loop(Socket) ->     %loop(Pid,Socket) 
    io:format("receiving...~n"), 
    receive
    	 %Msg  ->    io:format(" ~p~n",[Msg]);
        {tcp,Socket,Bin} ->
            io:format("i carlos am coming~n"), 
            %<<State:16,Str1:2/binary,Str2:2/binary>> = Bin,

           <<State:16,Date/binary>> = Bin,

           <<Size1:16,Date1/binary>> = Date,  %姓名的长度
           <<Str1:Size1/binary,Date2/binary>> = Date1,
           <<Size2:16,Date3/binary>> = Date2,  %密码的长度
           <<Str2:Size2/binary,Date4/binary>> = Date3,
           
           %io:format(" log2 ~p  ~p  ~n",[Str1,Str2]),
         
            %case binary_to_term(Bin) of
            case State of
                %登录
                   0000 -> %{Name,Passwd} = binary_to_term(Str),
                           
                            Name = binary_to_term(Str1),
                            io:format("logining  ~p ~n",[Name]), %cym
                            case info_lookup(Name) of
                                 [{user,Uid,Pname,Pwd,Logc,ChatC,Lastlog,LonginState}] -> S = term_to_binary("success"),
                                                                            N = term_to_binary(Name),
                                                                            Packet = <<0000:16,(byte_size(S)):16,S/binary,(byte_size(N)):16,N/binary>>, 
                                                                            % Packet = <<0000:16>>, 
                                                                            %处理一下业务， 登录次数加1 状态改为 1，登录时间在退出的时候才修改,
                                                                            %mysocket如果还没有添加socket就添加一下socket
                                                                            gen_server:call(server_example,{addSocket,Pname,Socket}),
                                                          					info_update(Pname,5,Logc+1),
                                                          					info_update(Pname,8,1),
                                                          					io:format("after logining ~p~n",[info_lookup(Pname)]),
                                                                            gen_tcp:send(Socket,Packet),
                                                                            io:format("user ~p have logged~n",[Name]), 
                                                                            % io:format("user ~p have logged ~p times ~n",[Name,Reply1]),
                                                                            loop(Socket);   %loop(Pid,Socket); 
                                %为空表示该用户没有记录    
                                [] -> io:format("you haved not registered yet"),   %返回的是[]  而不是  [{}]
                                        F = term_to_binary("failed"),
                                        N = term_to_binary(Name),
                                        Packet = <<0000:16,(byte_size(F)):16,F/binary,(byte_size(N)):16,N/binary>>,
                                        gen_tcp:send(Socket,Packet),
                                        loop(Socket)       % loop(Pid,Socket)
                            end;
                %接收信息
                    0001 ->
                            Name = binary_to_term(Str1),
                            Msg = binary_to_term(Str2),
                            [#user{chat_times=Ccount,state=LoginState}] = info_lookup(Name),
                            %更新聊天次数
                              case LoginState of
                              		1 ->  info_update(Name,6,Ccount+1),
                    			    	  %  Reply = personal_chat_count({cadd,Name,Ccount}),
                 	      				  %  io:format("User ~p :~p~n",[Name,Msg]),
                      		  			  %  io:format("User ~p have chatted with his friend on line ~p times ~n",[Name,Reply]),
                     				      N = term_to_binary({"ok","received"}),
                           				  Len = byte_size(N),
                          				  Packet = <<0001:16,Len:16,N/binary>>,
                           				  io:format("received  the  Msg  ~ts : ~ts~n",[Name,Msg]),
                           				  %广播信息  
                           				  gen_tcp:send(Socket,Packet),
                           				  gen_server:call(server_example,{sendAllMessage,Name,Msg}),
                          				  loop(Socket);  %loop(Pid,Socket);
                          		    0 ->  
                     				      N = term_to_binary({"failed","noLogin"}),
                           				  Len = byte_size(N),
                          				  Packet = <<0001:16,Len:16,N/binary>>,
                           				  io:format("user ~p  no login",[Name]),
                          				  gen_tcp:send(Socket,Packet),
                          				  loop(Socket)  %loop(Pid,Socket);
                          	  end;

                            
                %退出
                    0002 ->
                        Name = binary_to_term(Str2),    
                        io:format("see ~p: ~p~n",[Name,info_lookup(Name)]),
                        [#user{login_times=Log,last_login=LastLo}] = info_lookup(Name),    
                        Last = calendar:now_to_local_time(erlang:now()), % 4.格式化时间  todo 
                       % mysocket里，去除这个socket。
                        gen_server:call(server_example,{deleteSocket,Name,Socket}),
                        N = term_to_binary("ok"),
                        Packet = <<0002:16,(byte_size(N)):16,N/binary>>,
                        gen_tcp:send(Socket,Packet),
                        %修改最后登录时间
                        info_update(Name,7,Last),
                        info_update(Name,8,0),
                        io:format("after logout ~p~n",[info_lookup(Name)])
                        %io:format("~p users online~n",[Reply])



            end;

        {tcp_closed,Socket} ->
                              io:format("Server socket closed~n")
    end.