# diplomacy-server

Play the board game
[Diplomacy](https://en.wikipedia.org/wiki/Diplomacy_%28game%29) over HTTP.
Players may participate via their own private devices, so long as they have
network capabilities and a good web browser.

## Quick start

Start by installing the program.

```bash`
git clone git@github.com:avieth/diplomacy-server.git
cd diplomacy-server
cabal install
```

In order to get up and running you need a public/private key pair and
certificate, because the web
server uses secure HTTP. Here's how to create a key pair and self-sign a
certificate using OpenSSL.
Note that your browser will warn of an untrusted certificate when you connect
to the server.

```bash
openssl req -x509 -newkey rsa:2048 -keyout server.key -out server.pem -days 365 -nodes
```

When running the server, the key and certificate file locations can be
specified using `-k` and `-c` respectively; the defaults are `./key.pem` and
`./certificate.pem`. When running the server you must also give a username and
password for the administrator. These credentials allows you to create, start,
advance, pause, and destroy games.

```bash
./Main -u <username> -p <password>
```

The default port is `4347`, but this can be changed using `--port`. With
the server running, navigate to `https://<host>:4347/v1.0.0/diplomacy` to
begin playing via the simple but effective web client. The workflow is as
follows:

1. Administartor creates a game, choosing a password for it.
2. Players join the game using the administartor's chosen password.
3. Adminstrator starts the game.
4. Players use their own devices (smartphones, laptops) to view game state and
   issue orders.

The game will advance automatically unless the adminstrator pauses the game.
In this case, the game will advance only when the adminstrator explicitly
advances it, at which point the automatic advance feature kicks in again.
This is useful for putting a game on indefinite hiatus.

## The simple client

The file `client.html` is a barebones, not-so-user-friendly interface for
players and administrators (game-masters, if you will). It *should* provide
all necessary functionality to play a complete match, and *should* work
properly on all smartphones and personal computers with up-to-date web browsers.

To input typical and retreat phase orders using this client, one must type the
order object as it would be written in a pen-and-paper game; the map is not
interactive, but this would be a nice improvement.

A better HTML/JavaScript client, or even native iOS and Android clients,
would be *very* nice to have. Ideally, we could use the definitions of the
[diplomacy](https://github.com/avieth/diplomacy) Haskell library to produce
these clients. This way, we don't need to reproduce the basic definitions like
those related to provinces and their adjacency, and we could also use order
validation definitions to give helpful feedback when the user attempts to input
a new order (highlighting valid move targets or support subjects, for instance).

## A note on REST

This program uses the rest package, but it doesn't give a RESTful API; the
server *is* stateful. This package was chosen because it was relatively
quick and easy. It ought to be swapped out for something which

1. Does not claim to be RESTful.
2. Allows us to easily reuse handlers for HTTP and Email input.
