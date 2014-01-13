# Porter

Porter is an effort to ease dealing with user accounts and its related
functions: authentication and authorization. The root idea is not only
to have a library to help implementing this, but rather to have a
standalone service that you can simply use. So applications no matter
what language written in won't need to implement this again. Instead
they would send requests to the porter application.

Porter is divided into three parts:

* `api`: This module is a library with the least feasible
   dependencies. There are two dependencies for adding password
   encryption features:
   [bcrypt](http://www.mindrot.org/projects/jBCrypt/) and
   [scrypt](https://github.com/wg/scrypt). It can be used for
   embedding porter in your application and provides a simple http
   client for connecting to external porter instance. There are
   additional optional dependencies that are only needed for certain
   features (e.g. the http client).
* `app`: This module provides the porter application based on
  [akka](http://akka.io) and [spray](http://spray.io). Therefore it is
  possible to connect to this application via
  [akka remoting](http://doc.akka.io/docs/akka/2.2.3/scala/remoting.html). Besides
  this, there is a http rest endpoint for clients that are not build
  with [akka](http://akka.io) and a simple telnet interface for
  managing the application.
* `openid`:
  [OpenId](http://openid.net/specs/openid-authentication-2_0.html) is
  a specification to allow decentralized authentication. This module
  implements an OpenID Provider according to OpenId 2.0 (not
  everything is implemented yet, e.g. Relying Parties are not
  discovered as written in
  [chapter 13](http://openid.net/specs/openid-authentication-2_0.html#rp_discovery)
  and OpenId 1.1 compatibility is not done). It also provides some
  very basic html pages to register and manage your account.

The `dist` module packs all three together and creates a deployable
artifact -- a zip file containing the complete application.

## Concepts

The _realm_ here is a name for a distinct set of _accounts_ and
_groups_. An account (or group) does always belong to exactly one
realm. An account has a unique name, a set of secrets (e.g. passwords)
and a set of properties. The group also has a unique name and a set of
rules, which are either _permissions_ or _revocations_. An account can
be member of several groups. Accounts are needed for authentication
and accounts + groups are needed for authorization.


### Authentication

When authenticating, a given `Set[Credentials]` is validated against
the secrets of an account. The credentials must supply enough
information to lookup an account. Then the `Validator`s are consulted
which produces an `AuthResult` object. Each validator can "vote"
whether this credentials are correct or not. Or it may simply not
vote, if it cannot cope with the provided credentials.

Whether the `AuthResult` denotes a successful authentication or not is
up to client code. For example, some applications may want to check
certain properties of an account or may require the client to supply
at least two valid credentials.  The `Decider`, which is a function
`AuthResult => Boolean`, is meant to decide whether an `AuthResult`
presents a successful authentication. There are two implementations
provided. The most common one `OneSuccessfulVote` returns `true` if
there is at least on successful vote and no failed ones and the
account is not disabled.

### Authorization

Authorization is based on `Permission`s that can be associated to an
account via groups. Accounts have the permissions of all its groups
combined.  When receiving a permission, it can be verified whether it
is included in the permissions owned by the account.

A permission in porter is defined by the trait `Permission`, which
only defines one method:

```scala
def implies(other: Permission): Boolean
```

This looks the same as in most other permission classes, for example
`java.security.Permission`. It performs a simple subset check, to
determine whether the given permission is included or "implied" by
`this` permission. Each permission is required to have a string
representation. This is needed to pass permissions around the network
and to easily specify them. The `DefaultPermission` is taken from the
[shiro's wildcard permission](http://shiro.apache.org/permissions.html). The
permission string consists of parts that are separated by colon
"`:`". Each part can consist of multiple elements, which are separated
by comma "`,`". For example:

    part1:part2e1,part2e2,part2e3:part3:part4:*:part5

The meaning of each part is completely up to the application. The
`implies` works by comparing the parts of two permissions from left to
right. Then `p1 implies p2`, if the list of elements of a part of `p1`
is a super set of the elements of the corresponding part of `p2`. The
wild card `*` is used as a maximum value. Any list of elements is
a subset of `*`.

This makes it possible to define permission sets, like for example
`file:open:*`. Then `file:open:one.pdf` is implied by `file:open:*`,
since the `*` wild card matches any elements (and also any other
remaining parts).

The `ResourcePermission` is a special sub class of `DefaultPermission`
and reuses the notion of parts. The `ResourcePermission` must consist
of exactly three parts:

    domain:actions:uri-patterns

The *domain* part must be `"resource"`. The *actions* can be some
arbitary action strings, like `read`, `write` or `delete`. This is
again up to the application. For example: `resource:read:/folder1/**`
could be used to grant the `read` permission for all resources below
`/folder1/`. It is possible to specify more *uripatterns* separated by
comma.

Again wildcards can be used. The `ResourcePermission` supports `*`,
`**` and `?` wild cards.

While the wild cards allow you to define whole sets of permissions
quite easily, it is still hard to define certain sets. For example:
allow `resource:/main/**` but *not* `resource:/main/internal/**`. If
this is needed, a `Revocation` can be used to selectively revoke
permissions.

A revocation is the negation of a permission and is written with a
"`!`" in front of a normal permission definition. For example,
`!resource:read:/main/internal/**` means to revoke the permission
`resource:read:/main/internal/**`. With this you can first grant a
broader set of permissions and revoke certain subsets again. The rules

    resource:read:/main/**
    !resource:read:/main/internal/**

would grant read access to all resources below `/main/` but access to
resources below `/main/internal` is not allowed.

Porter will create two sets, one for all permissions and one for all
revocations. A permission that is in the first set and not in the
second is considered to be granted.

## Usage

There are two main usage scenarios:

1. External porter server
2. Embed porter in your application

### External

The advantage of an external porter application is that many
applications can use it for authentication, enabling single-sign-on
functionality.

You can connect via [akka](http://akka.io)'s remoting feature, by
looking up the porter actor or use the provided http rest
endpoint. The most decoupled way is to authenticate via OpenId.

#### Installation

Porter needs to be installed first. The default configuration expects
a running [MongoDB](http://www.mongodb.org/) server at its default
endpoint, so install mongodb first.  Then unzip the porter archive
somewhere, check the configuration file `porter.conf` inside the `etc`
directory and start porter using the script in `bin`.

This starts by default all interfaces:

* akka remote on port 4554
* http on port 6789
* telnet on port 2345
* openid on port 8888

For configuring, please look at the provided `porter.conf` file in the
`etc/` directory and change it accordingly. Here is a short overview
of the settings:

The first part
[configures akka](http://doc.akka.io/docs/akka/2.2.3/general/configuration.html)
to enable the remoting feature and enables debugging through slf4j.

    akka {
      actor {
        provider = "akka.remote.RemoteActorRefProvider"
      }
      remote {
        enabled-transports = [ "akka.remote.netty.tcp" ]
        netty.tcp {
          hostname = "127.0.0.1"
          port = 4554
        }
      }
      loggers = ["akka.event.slf4j.Slf4jLogger"]
      loglevel = "DEBUG"
    }

Then the porter setting follows. At first there is a list of
`Validator`s. Those are responsible for validating credentials against
a given account. By default, there are three defined:

* `PasswordValidator` validates passwords
* `DigestValidator` validates http digest authenticators
* `DerivedValidator` validates the special `DerivedCredentials` which
  can be created out of any other given secret. The intended use is to
  create an authenticator to be stored in cookies

You need to define the FQCN and its constructor arguments.

The store configuration defines all stores, read-only and mutable ones
by their FQCN. For mutable stores, you can additionally configure for
which realms they should be used. Maybe you want all accounts of one
realm to be saved in store A and others in store B. If nothing is
specified, the first mutable store will be used.

The last part to configure are `PermissionFactory`s. They can create
`Permission` objects from strings. If you need other permission
implementations than provided, you could define them here.

All remaining configuration regards the interfaces: telnet, http and
openid.

#### Telnet

The telnet interface is for administrative tasks. The port should be
protected, of course.

In a terminal, type

    $telnet localhost 2345
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

    Welcome to porter 0.1.0

    Type 'help' for a list of available commands.

    porter>

The `help` command lists all available commands.

#### http

This interface is intended for clients that are not built with
[akka](http://akka.io) (and maybe can't use `porter-api`
artefact). This http server accepts json formatted http requests and
responds with json.

For example, an authentication request is as follow:

    curl -i -X POST -d '{ "realm": "default", "creds": [{ "accountName": "eike", "password": "test"}] }' -H "Content-Type: application/json" http://localhost:6789/api/authc

For convenience, there is a simple http client class
`porter.client.http.PorterHttp` that does the (un)marshalling for you.

#### akka remote

If your application is build on [akka](http://akka.io), just lookup the remote porter actor.
The `Porter` extension can help with this:

```scala
Porter(system).select()
```

The porter messages are defined in `Porter.Messages`, authenticating a
user could be as follows:

```scala
import Porter.Messages.authc._
val porter = Porter(system).select()
(porter ? Authenticate("realm", Set(PasswordCredentials("eike", "test"))).mapTo[AuthenticateResp]
```

The `PorterUtil` object defines a set of utility functions around the
basic porter messages. The `authenticationFuture` in there also
updates account properties based on the result.

When using the [spray](http://spray.io) toolkit, the
`PorterDirectives` trait may be useful.

#### OpenId

[OpenId](http://openid.net/specs/openid-authentication-2_0.html) is
the most standardized form of authenticating with porter. The downside
is, that it is much more complicated than the `http` interface, for
example, because it enables arbitrary parties to exchange
authentication information securely. If openid is enabled, porter is
an OpenId Provider and clients need to follow the OpenId spec when
authenticating users. That also enables you to use your own porter
instance to authenticate with any website that supports OpenId
authentication.

How to authenticate using the OpenId scheme is out of scope for this
document. The OpenId identifier is by default the url to the porter
openid endpoint with the account name appended, like
`https://mydomain.com/<account>`. This can be changed via the
configuration.

Authorization cannot be performed using openid.


### Embedding

When your application is build with [akka](http://akka.io), you can
pull in the `app` module and create new porter actor. Otherwise, pull
in the `api` dependency and implement the needed glue code for your
application.

#### With Akka

The [akka](http://akka.io) extension `porter.app.akka.Porter` can be
used to create a new porter actor:

```scala
class MyActor extends akka.actor.Actor {

  val porter = Porter(context.system).createPorter(context)

}
```

This will use the default configuration to create the actor. Then you
can pass messages from `porter.client.Messages`.


#### Without Akka

When not using [akka](http://akka.io), there is the trait
`porter.api.auth.Porter` that combines all porter features. You need
to implement the missing parts, by at least providing a `Store` that
contains account information. Optionally, you can provide a mutable
store, different set of validators or a custom permission factory by
overriding corresponding methods:

```scala
class MyPorter extends Porter {
  def store: Store = ???

  override def validators: Iterable[Validator] = super.validators

  override def mutableStore(realm: Ident): Option[MutableStore] = super.mutableStore(realm)

  override def permissionFactory: PermissionFactory = super.permissionFactory
}
```

The package `porter.store` contains `Store` skelletons and an
implementation based on java properties files.
