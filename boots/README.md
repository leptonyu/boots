# boots

[![Hackage](https://img.shields.io/hackage/v/boots.svg?logo=haskell)](https://hackage.haskell.org/package/boots)
[![Build](https://img.shields.io/travis/leptonyu/boots.svg?logo=travis)](https://travis-ci.org/leptonyu/boots)
[![stackage LTS package](http://stackage.org/package/boots/badge/lts)](http://stackage.org/lts/package/boots)
[![stackage Nightly package](http://stackage.org/package/boots/badge/nightly)](http://stackage.org/nightly/package/boots)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/leptonyu/boots/blob/master/boots/LICENSE)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/boots)

IoC Monad in Haskell.

### Packages based on boots

- [![Hackage](https://img.shields.io/badge/boots-app-orange)](https://hackage.haskell.org/package/boots-app) Factory for quickly building an application.
- [![Hackage](https://img.shields.io/badge/boots-web-orange)](https://hackage.haskell.org/package/boots-web) Factory for quickly building a web application.
- [![Hackage](https://img.shields.io/badge/boots-cloud-orange)](https://hackage.haskell.org/package/boots-cloud) Factory for quickly building a microservice.


### Motivation

Simplify to create an application in Haskell.

When we decide to create an application using Haskell. We may need using configurations, loggers as basic functions. If this application needs storages, caches, etc., then we have to weaving the management of connection of these facilities into the application. Connections need to be created before and be destroyed after using them. There is a common strategy to manage connections, that is using `Control.Monad.Cont`. Then we can encapsulate the management of connections separately. For example, we can write a database factory `Factory m cxt DBConnection`, which can manage the database connections in monad `m` with context `cxt`. Context `cxt` may be requested for configurations or logging functions. When all the components of application are encapsulated by plugins, then building an application will be simplified.

## A Project Use boots to Build

Refer to [鬼谷子](https://github.com/leptonyu/guiguzi)
