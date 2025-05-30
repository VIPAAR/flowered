Flowered - A Node-RED backend coded in Erlang
=====

This is a fork of [Erlang-Red](https://github.com/gorenje/erlang-red)
to convert it into a simple erlang library for just running flows.

This fork removes all the UI and Web Servers (which means you can't
use Http In or other servers).

It is expected that you'll already have a framework to handle incoming
request, and can kick off pre-existing flows manually.


Supported Nodes & Features
---

This is a non-complete list of [nodes](src/nodes/) that partially or completely work:

| Node | Comment |
| ---- | ------- |
| [catch](src/nodes/ered_node_catch.erl) | catches exception of selected nodes and of entire flows but not groups |
| [change](src/nodes/ered_node_change.erl) | supports many operators but not all. JSONata in basic form is also supported. |
| [complete](src/nodes/ered_node_complete.erl) | is available and can be used on certain nodes, not all |
| [csv](src/nodes/ered_node_csv.erl) | initial RFC4180 decoder working, supports only comma separator |
| [debug](src/nodes/ered_node_debug.erl) | only debugs the entire message, individal msg properties aren't supported. msg count as status is supported. |
| [delay](src/nodes/ered_node_delay.erl) | supported static delay not dynamic delay set via `msg.delay` |
| [exec](src/nodes/ered_node_exec.erl) | executing and killing commands is supported  but only for commands in spawn mode and set on the node. Appending arguments to commands isn't supported. Timeouts are supported. Kill messages are also supported. |
| [file in](src/nodes/ered_node_file_in.erl) | working for files located in `/priv` |
| [function](src/nodes/ered_node_function.erl) | working for any Erlang. Stop and start also respected. Timeout and more than one output port isn't supported. |
| [http in](src/nodes/ered_node_http_in.erl) | working for GET and POST, not available for PUT,DELETE etc |
| [http request](src/nodes/ered_node_http_request.erl) | basic support for doing rrequests, anything complex probably won't work |
| [http response](src/nodes/ered_node_http_response.erl) | working |
| [inject](src/nodes/ered_node_inject.erl) | working for most types except for flow, global ... |
| [join](src/nodes/ered_node_join.erl) | *manual arrays of count X* is working, `parts` isn't supported  |
| [json](src/nodes/ered_node_json.erl) | working |
| [junction](src/nodes/ered_node_junction.erl) | working |
| [link call](src/nodes/ered_node_link_call.erl) | working - dynamic calls also |
| [link in](src/nodes/ered_node_link_in.erl) | working |
| [link out](src/nodes/ered_node_link_out.erl) | working |
| [markdown](src/nodes/ered_node_markdown.erl) | working and supports whatever [earmark](https://github.com/pragdave/earmark) supports. |
| [mqtt in](src/nodes/ered_node_mqtt_in.erl) | should be working |
| [mqtt out](src/nodes/ered_node_mqtt_out.erl) | should be working |
| [noop](src/nodes/ered_node_noop.erl) | doing nothing is very much supported |
| [split](src/nodes/ered_node_split.erl) | splitting arrays into individual messages is supported, string, buffers and objects aren't. |
| [status](src/nodes/ered_node_status.erl) | working |
| [supervisor](src/nodes/ered_node_supervisor.erl) | Erlang-only node that implements the [supervisor behaviour](https://www.erlang.org/doc/system/sup_princ.html). Supports supervising supervisors and ordering of processes (i.e. nodes) to ensure correct restart and shutdown sequences. |
| [switch](src/nodes/ered_node_switch.erl) | most operators work along with basic JSONata expressions  |
| [template](src/nodes/ered_node_template.erl) | mustache templating is working but parsing into JSON or YAML isn't supported |
| [trigger](src/nodes/ered_node_trigger.erl) | the default settings should work |

- Contexts are **not supported**, so there is no setting things on `flow`, `node` or `global`.

- [JSONata](https://jsonata.org) has been **partially implemented** by the [Erlang JSONata Parser](https://github.com/gorenje/erlang-red-jsonata).

Elixir & Erlang-RED
---

Elixir helpers can be added to [erlang-red-elixir-helpers](https://github.com/gorenje/erlang-red-elixir-helpers) repository.

There is nothing stopping anyone from creating a complete node in Elixir provided there is a Erlang "node-wrapper", i.e., a bit of Erlang code in the [src/nodes](src/nodes) directory that references the Elixir node.

The initial example [markdown node](https://github.com/gorenje/erlang-red/blob/42f10112baac5a5f916ecd805eafc87382632dec/src/nodes/ered_node_markdown.erl#L38) is an Erlang node that references Elixir code. I also wrote an Elixir wrapper function whereby I could have just as easily referenced Earmark directly from the Erlang code. That was a stylist choice.

I intend to use Elixir code for importing Elixir libraries to the project and less coding nodes in Elixir. I simply prefer Erlang syntax. But each to their own :)

Build
-----

    $ rebar3 get-deps && rebar3 compile

Test
-----

    $ rebar3 eunit

Development
---

    $ rebar3 shell --apps erlang_red


Usage
----

Add the following to your `mix.exs`:

```
     {:erlang_red_helpers, git: "https://github.com/gorenje/erlang-red-elixir-helpers", tag: "0.1.3", override: true},
     {:flowered, git: "https://github.com/VIPAAR/flowered", tag: "1.0.0"}
```

To run a flow:

```elixir
# Have a flow as a string of json.
# It is recommended to use an `inject` node
#  as your starting point
flow = "..."

# Generate a unique name
ws_name = Node.self()

# Start up the nodes in the flow (if they aren't started)
:ered_compute_engine.deploy(flow, ws_name)

# Use the `inject` node id from our flow
#  to kick everything off
injector_id = "get-this-from-your-flow"
{:ok, injector_pid} = :ered_nodes.nodeid_to_pid(wsname, injector_id)

# Create an outgoing message
{:outgoing, msg} = :ered_msg_handling.create_outgoing_message(ws_name)

# You can inject any parameters you want into this message
#  Common parameters are the body, params, cookies, and query,
#  especially if this is originating from a web request.
#
# This is OPTIONAL
#
# We will generate a map, and put this under the `:req` key
# 
# Note: it is important to use `charlist` for keys, or any templates
#  that mustache will not work!
request_object = (%{
      String.to_charlist("body") => body,
      String.to_charlist("params") => strings_to_lists(path_params),
      String.to_charlist("cookies") => [],
      String.to_charlist("query") => strings_to_lists(query_params),
})
msg = Map.put(msg, :req, request_object)

# Put our pid into the msg as `reqpid`. This is critical
#  as the output is going to be sent back to registered
#  process
msg = Map.put(msg, :reqpid, self())

# Start the flow
GenServer.cast(injector_pid, {:outgoing, msg})

# Listen for a response. Depending on
#  our final node, you might get different messages.
# In this case, our final node is a Http Response.
receive do
  {:reply, status, headers, ^ws_name, body} ->
    {:ok, status, headers, body}
  after 10_000 ->
    # timeout with an error after 10 seconds
    {:error, :timeout}
end
```

Sibling Repos
---

An overview of the sibling projects for both the reader and me:

- [Erlang-Red](https://github.com/gorenje/erlang-red) is the original
  project this was forked from. We may rebase upon erlang-red every
  once in a while.

- [JSONata support for
  Erlang-RED](https://github.com/gorenje/erlang-red-jsonata) is
  implemented by an Erlang parser with a grammer that covers most of
  JSONata syntax, no guarantees made. Support of JSONata functionality
  is limited to what the test flows require. Nothing prevents others
  from extending the functionality themselves, it is not a priority of
  mine.

- [Elixir helper
  library](https://github.com/gorenje/erlang-red-elixir-helpers)
  allows Elixir code to be also part of Erlang-RED. Erlang-RED is not
  intended to be a *pure* Erlang project, it is intended to be a
  *pure* BEAM project. Anything that compiles down to the BEAM VM, why
  not include it?


Acknowledgement
---

[Gorenje](https://github.com/gorenje) for the original implementation.

License
---

`DONT'T DO EVIL`

Also be aware that this project partly uses the *Don't do Evil*
un-enforceable license. The point of the license is not to be
enforceable but to make the reader think about what is evil. 

* (c) 2025 Gerrit Riessen - https://github.com/gorenje
* (c) 2025 Help Lightning - https://helplightning.com
