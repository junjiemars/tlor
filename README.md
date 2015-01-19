Tlor
====
Tlor is a distributed erlang's otp app, which can do some basic
ejabberd 2.1.13+ xmpp server's mangement jobs.

## Requirements
1. [Erlang R16B+](https://github.com/erlang/otp.git)
2. [Rebar 2.5.1+](https://github.com/rebar/rebar.git) 
3. Make tools, the one in your box is fine

## Configurations
1. In rel/files/sys.config, replace all <your-*> with your extractly settings.
`{host_ipv4, "<your-host-ipv4"}` specify the ipv4 address of the 
ejabberd server.
`{host_port, <your-host-port>}` specify the port, default is 5222.
`{pubsub_service, "<your-pubsub-node>"}` specify the pubsub's node name,
the format like: pubsub.abc.def
2. In rel/files/vm.args, replace all <your-*> with yours.
`-name tlor@<your-tlor-ipv4>` specify tlor the ipv4 address which run on.
`-setcookie <your-tlor-cookie>` specify the cookie.

## How to build
1. to compile: make compile
2. to make distribution: make release

## Hot to start/stop/debug
1. start: `rel/tlor/bin/tlor start`
2. stop: `rel/tlor/bin/tlor stop`
3. debug: `rel/tlor/bin/tlor console`

## What's you can do
1. Register new user
2. Publish to the owner node with any content
3. Say to somebody just in time
4. Something else your want it then found it
