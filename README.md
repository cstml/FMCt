# FMCt

Typed Functional Machine Calculus.

The project is in active development, and there will be many breaking changes.

## Building

To build there are two ways, depending on which you prefer. Note that both are
valid for UNIX systems. As it stands, there is no defined way of building on
windows except for the normal `cabal new-build`.

### With Nix

1. pull the repo,
2. run: `nix-build`,
3. you now have access to bothe executables in `/result`

### With Cabal

1. pull the repo,
2. run: `cabal new-run FMCt-web`,
3. you are locally serivng the FMCt-web at port 8080 
4. if your port is busy, set the FMCt-web to serve at a new port by setting
`export PORT=8082` to 8082 or whichever port you want to serve from.

#### With GNUMake

Have a look inside the `makefile` for useful commands. 

## Documentation

To create the documentation you need to run `make documentation`.

# FMCt-web

The FMCt-web is a web interface for the FMCt where people can try out the
language and its type checker without having to install anything.

There's also a CI/CD which deploys the main branch to [this website](https://fmct-web.herokuapp.com/).
