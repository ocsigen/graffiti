| Branch master                   |
| ------------------------------- |
| [![Graffiti][1]][2] |

[1]:  https://github.com/ocsigen/graffiti/actions/workflows/master.yml/badge.svg
[2]:  https://github.com/ocsigen/graffiti/actions

## Graffiti is a collaborative drawing application.

It is the example application of the Ocsigen tutorial, for Eliom 13:

- `simple/`: the application of the tutorial
  [Writing a client/server Eliom application](https://ocsigen.org/tuto/latest/application.html),
- `extended/`: the multi-user version of the tutorial
  [Traditional web interaction in a client-server app](https://ocsigen.org/tuto/latest/misc.html).

Both were created with `eliom-distillery -template app`.

Requirements
------------

Binaryen 120 or later (to compile the client to WebAssembly), then, from
the directory of the application:

    opam install --deps-only .

Usage
-----

From the directory of the application:

    dune build
    dune exec graffiti

and go to http://localhost:8080/. See the `README.md` file of each
application for the other ways to run it.
