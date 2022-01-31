Strategy Games API written in scala for [playstrategy.org](https://playstrategy.org) [![Build Status](https://travis-ci.org/Mind-Sports-Games/strategygames.svg?branch=master)](https://travis-ci.org/Mind-Sports-Games/strategygames)

It is entirely functional, immutable, and free of side effects.

strategygames is a fork from [scalachess](https://github.com/lichess-org/scalachess) which is the equivalent Chess API for [lichess.org](https://lichess.org).

INSTALL
-------

Clone scalachess

    git clone git://github.com/Mind-Sports-Games/strategygames

Get latest sbt on http://www.scala-sbt.org/download.html

Start sbt in scalachess directory

    sbt

In the sbt shell, to compile scalachess, run

    compile

To run the tests (with coverage):

    clean coverage test
    coverageReport

Code formatting
###

This repository uses [scalafmt](https://scalameta.org/scalafmt/).

Please [install it for your code editor](https://scalameta.org/scalafmt/docs/installation.html)
if you're going to contribute to this project.

If you don't install it, please run `scalafmtAll` in the sbt console before committing.
