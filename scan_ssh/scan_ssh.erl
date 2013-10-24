-module(scan_ssh).

-export([host_responds_to_ssh/1]).

-define(TIMEOUT, 1000).

main([Address, _]) ->
  Bits = 24,
  case extract_subnet(Address, Bits) of
    {ok, BaseAddress} ->
      scan_subnet(BaseAddress, Bits);
    {error, ipv6} ->
      io:format("This program does not handle IPv6 addresses~n");
    {error, unparsable} ->
      io:format("failed to parse address: ~p~n", [Address]),
      usage()
  end;

main(_) ->
  usage().

extract_subnet(Address, Bits) ->
  case inet_parse:address(Address) of
    {ok, {I1, I2, I3, I4}} ->
      Base = extract_ipv4_subnet({I1, I2, I3, I4}, Bits),
      {ok, Base};
    {ok, _} ->
      {error, ipv6};
    true ->
      {error, unparsable}
  end.

scan_subnet(BaseAddress, Bits) ->
  check_host("192.168.1.2").

check_host(Host) ->
  Found = host_responds_to_ssh(Host),
  case Found of
    yes ->
      io:format("~p\n", [Host]);
    no ->
      false
  end.

host_responds_to_ssh(Host) ->
  crypto:start(),
  ssh:start(),
  Result = ssh:connect(Host, 22, [{silently_accept_hosts, true}, 
                                  {user_interaction, false}, 
                                  {user, "user"}, 
                                  {password, "password"},
                                  {timeout, ?TIMEOUT}]),
  HostResponding = case Result of
    {ok, Connection} ->
      io:fwrite("connected\n"),
      ssh:close(Connection),
      yes;
    {error, ehostunreach} ->
      io:fwrite("host unreachable\n"),
      no;
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      yes;
    _ ->
      io:fwrite("unexpected error\n"),
      no
  end,
  crypto:stop(),
  ssh:stop(),
  HostResponding.

usage() ->
  io:fwrite("escript scan_ssh.erl IP_ADDRESS SUBNET_BITS\n").

