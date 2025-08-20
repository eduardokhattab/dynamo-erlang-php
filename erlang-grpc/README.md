erlang_grpc
=====

An OTP application

This is a gRPC TCP Server

Build
-----

    $ rebar3 get-deps && rebar3 compile

Prerequisites
-----
Make sure to create all the needed AWS resources with the CloudFormation Template. The process is described on the [previous README.md](../README.md)

Once you have created the resources, get the **AWS_ACCESS_KEY_ID** and **AWS_SECRET_ACCESS_KEY** to execute the app.

Execution
-----

### Server

    $ AWS_ACCESS_KEY_ID="<paste here>" AWS_SECRET_ACCESS_KEY="<paste here>" rebar3 shell

    1> erlang_grpc_server:start_link().

### Client Test

I've created a simple gRPC client to test the application. The client has two methods which save and retrieve encrypted data from the DynamoDB:

    $ rebar3 shell

    1> erlang_grpc_client:set("key", "value").
    2> erlang_grpc_client:get("key").

To analyze the data encrypted, check the DynamoDB table on AWS

#### Data bigger than 4kb

In order to save data bigger than 4kb, this application is using KMS Data Keys instead of the common Customer Managed Keys (CMK) encrypt/decrypt functions. To test the functionality, use the [big file content](../big_content_value.txt) as a payload value. It contains more than 4kb of size.

    $ rebar3 shell

    1> erlang_grpc_client:set("key", "<content>").
    2> erlang_grpc_client:get("key").

Discussion
-----
1. What are the limits of operation of the service (ex: concurrent users,
memory usage)?

> The main bottleneck is due to the AWS Request quotas. Especially the KMS, we are calling GenerateDataKey and encrypt/decrypt/per request. In a scenario of high concurrency, we could start to see throttled requests. Also, creating a process per connection is consuming more memory and limiting the number of concurrent users.

2. The implementation is scalable, i.e., it’s able to handle a large number of
concurrent users on multiple servers? How can/did I achieve scalability, software
and infrastructure wise?

> The way it is, the server is creating a process/per request. In a production environment, we could refactor it to use a worker pool of connections. Regarding the AWS quotas, we could cache the Data Key to avoid hitting the maximum limit of requests per second.
Also, in an infra-wise, we could deploy N servers to handle the requests, with a load balancer at the front. For example, deploying the application on a Kubernetes cluster for horizontal scaling.

3. What was more challenging in the development of the service?

> Learning the Erlang syntax/workflow. Besides that, there isn't a big number of examples available on the internet.
> Working with crypto algorithms
> Debug Erlang code

4. If I had infinite time how would I improve the service?

> - Create automated tests (unit/integration)
> - Use a cache or reuse the KMS Data Key for encryption
> - Set a rate-limit for KMS
> - Creating a worker pool of connections
> - Refactor the TCP server to enable Keep-Alive.
> - Expose metrics for monitoring
> - Performance and profilling tests
> - Improve documentation
> - Use env variables

