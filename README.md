# Simple OCaml UDP Server

Simple chat application between CLI client users in OCaml. Utilizes promises and leverages functional programming
paradigms to deal with concurrent requests and scaling.

WIP: Currently in development.

Project created as a part of COMP 302 Programming Languages and Paradigms at McGill.

## Features

This repository allows you to build executables for both a server and a client that connects. Multiple clients can connect to one server instance.
Messages are then synced acrossed the clients.

- Server and Client works on any client instance connected to localhost
- Server and Client works on the internet given correct IP configurations using environment variables
- Username Customization
- Simulating concurrency with use of promises and monads

## Usage

This chat application has only been tested on **UNIX-based systems**. There is no guarantee things will work if you are not on UNIX.
If you are on Windows, consider using WSL. By default, the application only works on `localhost`.

**Prerequisite** tools installation required:

1. OCaml
2. OPAM (OCaml Project Manager)
3. [Dune](https://dune.build/install)

If you only have OPAM and not dune, you can run the following to install Dune & associated project packages.

```
$ opam install dune ocaml lwt logs logs-fmt
```

### Build the application on your computer

First, clone the repo and open the `root` using Bash or Bash-like shells.

Use Dune to build the project (required libraries should be configured in the `Dune` files already).

```
$ dune build
```

Dune should then build required binaries for you to access the chat application functionalities.

To boot up a server for hosting the chat application:

```
$ dune exec ./_build/default/bin/server.exe
```

The server will accept clients from any address.

To boot up a client to access given server:

```
$ dune exec ./_build/default/bin/client.exe -h "SERVER_IP_ADDRESS" -p PORT

-h { Server IP Address | Default: Localhost }
-p { Listening Port | Defualt: 9000 }
```

**You should have a working CLI chat application!**
