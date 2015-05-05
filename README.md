# Traffic light

### Requirements:

- erlang R16B or higher with mnesia.
- git 

### Build:

Using `riak` to get dependencies and build application.

Project structure is standard for riak applications.

Application config (such as port) located in `src/traffic_light.app.src`

Make tasks to quickly build and run application:

- make deps - get dependencies
- make compile - compile application
- make console - start application in erlang console
- make tests - run tests (eunit and common_test)
