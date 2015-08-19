<img src="http://www.braveterry.com/wp-content/uploads/2015/03/swagger2.png"/>

# cowboy-swagger
[Swagger](http://swagger.io/) integration for [Cowboy](https://github.com/ninenines/cowboy) (built on [trails](https://github.com/inaka/cowboy-trails)).

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](https://www.hipchat.com/gpBpW3SsT).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Why Cowboy Swagger?
Simple, because there isn't a tool in Erlang to document Cowboy RESTful APIs easy and fast,
and to improve development productivity.

With `cowboy_swagger` is possible to integrate Swagger to your Erlang projects that use Cowboy as a web server.
It is extremely easy to use, and with just a few steps you'll have a nice Web documentation for your RESTful APIs.

To learn a bit more about Swagger, please check this [blog post](http://inaka.net/blog/2015/06/23/erlang-swagger-2015/).

## How to Use it?
This is the best part. It is extremely easy.

### 1. Document each Cowboy Handler
Because `cowboy_swagger` runs on top of `trails`, the first thing that you have to do
is document all about your handler within the trails metadata. Keep in mind that
all fields defined within each method into the metadata must be compliant with the
[Swagger specification](http://swagger.io/specification).

For example, suppose that you have `example_echo_handler`, so it must implement the `trails/0`
callback from `trails_handler` behaviour:

```erlang
trails() ->
  Metadata =
    #{get =>
      #{tags => ["echo"],
        description => "Gets echo var from the server",
        produces => ["text/plain"]
      },
      put =>
      #{tags => ["echo"],
        description => "Sets echo var in the server",
        produces => ["text/plain"],
        parameters => [
          #{name => <<"echo">>,
            description => <<"Echo message">>,
            in => <<"path">>,
            required => false,
            type => <<"string">>}
        ]
      }
    },
  [trails:trail("/message/[:echo]", example_echo_handler, [], Metadata)].
```

To get a better idea of how your handler should look like, please check [here](./example/src/example_echo_handler.erl).

### 2. Include cowboy_swagger in your app
First, you need to include `cowboy_swagger_handler` module in your list of trails to be compiled.

```erlang
% Include cowboy_swagger_handler in the trails list
Trails = trails:trails([example_echo_handler,
                        example_description_handler,
                        cowboy_swagger_handler]),
% store them
trails:store(Trails),
% and then compile them
Dispatch = trails:single_host_compile(Trails),
```

The snippet of code above is usually placed when you start `cowboy`. Check it [here](./example/src/example.erl#L31).

Then add `cowboy_swagger` to the list of apps to be loaded in your `*.app.src` file.

```erlang
{application, example,
 [
  {description, "Cowboy Swagger Basic Example."},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    jiffy,
    cowboy,
    trails,
    cowboy_swagger
   ]},
  {modules, []},
  {mod, {example, []}},
  {registered, []},
  {start_phases, [{start_trails_http, []}]}
 ]
}.
```

And that's it, you got it. Now start your application and then you will have access to the API docs
under the path `/api-docs`. Supposing that you're running the app on `localhost:8080`,
that will be [http://localhost:8080/api-docs](http://localhost:8080/api-docs).

## Example
For more information about `cowboy_swagger` and how to use it, please check this [Example](./example).
