lanark
===

[![Maven Central](https://img.shields.io/maven-central/v/com.io7m.lanark/com.io7m.lanark.svg?style=flat-square)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.io7m.lanark%22)
[![Maven Central (snapshot)](https://img.shields.io/nexus/s/com.io7m.lanark/com.io7m.lanark?server=https%3A%2F%2Fs01.oss.sonatype.org&style=flat-square)](https://s01.oss.sonatype.org/content/repositories/snapshots/com/io7m/lanark/)
[![Codecov](https://img.shields.io/codecov/c/github/io7m-com/lanark.svg?style=flat-square)](https://codecov.io/gh/io7m-com/lanark)
![Java Version](https://img.shields.io/badge/17-java?label=java&color=e65cc3)

![com.io7m.lanark](./src/site/resources/lanark.jpg?raw=true)

| JVM | Platform | Status |
|-----|----------|--------|
| OpenJDK (Temurin) Current | Linux | [![Build (OpenJDK (Temurin) Current, Linux)](https://img.shields.io/github/actions/workflow/status/io7m-com/lanark/main.linux.temurin.current.yml)](https://www.github.com/io7m-com/lanark/actions?query=workflow%3Amain.linux.temurin.current)|
| OpenJDK (Temurin) LTS | Linux | [![Build (OpenJDK (Temurin) LTS, Linux)](https://img.shields.io/github/actions/workflow/status/io7m-com/lanark/main.linux.temurin.lts.yml)](https://www.github.com/io7m-com/lanark/actions?query=workflow%3Amain.linux.temurin.lts)|
| OpenJDK (Temurin) Current | Windows | [![Build (OpenJDK (Temurin) Current, Windows)](https://img.shields.io/github/actions/workflow/status/io7m-com/lanark/main.windows.temurin.current.yml)](https://www.github.com/io7m-com/lanark/actions?query=workflow%3Amain.windows.temurin.current)|
| OpenJDK (Temurin) LTS | Windows | [![Build (OpenJDK (Temurin) LTS, Windows)](https://img.shields.io/github/actions/workflow/status/io7m-com/lanark/main.windows.temurin.lts.yml)](https://www.github.com/io7m-com/lanark/actions?query=workflow%3Amain.windows.temurin.lts)|

## Lanark

A specification for restricted dotted names.

## Motivation

Applications such as compilers and package systems often use so-called
[reverse dns notation](https://en.wikipedia.org/wiki/Reverse_domain_name_notation)
to identify packages and code artifacts. Unfortunately, due to reverse DNS
notation being underspecified, each implementation has its own idea of which
names should be permitted and which should be rejected.

This specification attempts to define a restricted form of the notation with the
following properties:

  * Names can be validated with a simple regular expression that is defined
    in such a way as to avoid resource exhaustion attacks.
  * Names are defined using a strict subset of [ASCII](https://en.wikipedia.org/wiki/ASCII)
    in order to avoid Unicode-based spoofing and phishing attacks.
  * Names are defined such that the maximum length of a name is bounded
    in order to provide for predictable storage use in database applications.

## Definitions

### Regular Expression

A _dotted name_ is a string matching the following regular expression:

```
([a-z][a-z0-9_-]{0,63})(\.[a-z][a-z0-9_-]{0,62}){0,15}
```

#### Theorem SPLIT

A _dotted name_ can always be split into between 1 and 16 _segments_
by splitting the name into separate parts at each dot, such that each part is
a valid _dotted name_ .

#### Proof

Let `x` be a _dotted name_. By the definition of the
[regular expression](#regular-expression) that defines a dotted name, `x`
must be one of:

  * A single character in the range `[a-z]`.
  * A _primary segment_ consisting of a single character in the range `[a-z]`
    followed by up to 63 characters from the set `[a-z0-9_-]`.
  * A _primary segment_ followed by between `1` and `15` _secondary segments_
    that each consist of a dot `.`, followed by a single character in the range
    `[a-z]`, followed by up to 62 characters from the set `[a-z0-9_-]`.

If `x` is single character in the range `[a-z]`, then there is no splitting
to be performed and `x` is already trivially matched by the regular expression
and is therefore a valid dotted name.

If `x` consists of a single _primary segment_, then there is no splitting
to be performed and `x` is already trivially matched by the regular expression
and is therefore a valid dotted name.

If `x` consists of a _primary segment_ followed by between `1` and `15`
_secondary segments_, then for each segment `s` it is necessary to show that
`s` matches the regular expression when the preceding dot (if `s` is a
_secondary segment_) is removed.

  * If `s` is a _primary segment_, then it already matches the regular
    expression.

  * If `s` is a _secondary segment_, then it will effectively become the
    _primary segment_ of the new dotted name. Because `s` is
    a _secondary segment_, it must match the subexpression
    `[a-z][a-z0-9_-]{0,62}`. By the semantics of length ranges in
    regular expressions, any string matched by an expression `e{0,n}` will
    also be matched by an expression `e{0,n+1}`. As the
    subexpression for _primary segments_ is `[a-z][a-z0-9_-]{0,63}`, `s`
    will match and is therefore a valid _primary segment_.

#### Theorem SIZE

The number of characters in any _dotted name_ is `<= 1024`.

#### Proof

By [SPLIT](#theorem-split), we know that a dotted name `x` consists of
a _primary segment_ followed by up to `15` _secondary segments_.

The regular (sub)expression that matches a _primary segment_ is
defined as `[a-z][a-z0-9_-]{0,63}`. The longest size of a _primary segment_
is therefore `1 + 63 = 64`.

The regular (sub)expression that matches a _secondary segment_ is
defined as `\.[a-z][a-z0-9_-]{0,62}`. The longest size of a _secondary segment_
is therefore `1 + 1 + 62 = 64`.

The regular (sub)expression that defines how many _secondary segments_ may
appear in a _dotted name_ is defined as `e{0,15}`, so the maximum number of
_secondary segments_ is `15` and therefore the maximum number of characters
that can be used for _secondary segments_ is `15 * 64 = 960`.

We can therefore conclude that a string consisting of a maximum length
_primary segment_ and the maximum number of maximum length
_secondary segments_ is `64 + (15 * 64) = 1024`.

#### Coq

Machine-checked proofs of the above propositions are provided in the
[Lanark.v](com.io7m.lanark.core/src/main/resources/com/io7m/lanark/core/Lanark.v) file.

## Rationale

_Why are names defined in terms of a regular expression rather than as a BNF
grammar_?

This specification is being written to support the development of various
[io7m](https://www.io7m.com) software packages, and validation of dotted names
is expected to occur in a wide range of different contexts such as XML schemas,
at runtime in Java code, in definitions of SQL tables, and etc. These environments
all feature regular expression validation, and not all of them support writing
parsers for more advanced grammars. With the specification itself containing
the canonical regular expression, this expression can literally be pasted into
various locations without needing any changes.

_Why are names restricted to a subset of ASCII?_

One of the uses for dotted names is in the naming of software packages published
onto the web. In systems that allow for the full use of Unicode to name
packages, it's possible for malicious parties to spoof the appearance of
packages by using carefully crafted names. For example:

  `com.io7m.example`
  `com.iọ7m.example`

The second package is a malicious package. It would be fairly trivial for
someone to sneak in a reference to this package as a dependency in an open
source project and have it go unnoticed. For those unable to tell the
difference: The `o` in `io7m` in the second package is actually `U+1ECC`
("Latin Capital Letter O with Dot Below"). This is almost indistinguishable
from the first package, but could easily be used to fool people into thinking
they're installing a package written by someone controlling the `com.io7m`
namespace.

_Won't ASCII cause problems for non-English developers?_

Currently, [Maven Central](https://search.maven.org/) is the largest collection
of open-source software on the planet. Artifacts published to Maven Central
have a _group name_ and an _artifact name_. It is conventional for _group names_
to be in reverse DNS notation, and it is not uncommon for _artifact names_
to also be in this same notation. By analyzing the largest collection of
of open-source software on the planet, we can probably get some idea as to how
developers all over the world are naming their artifacts.

An [index](https://repo1.maven.org/maven2/.index/) is published weekly consisting
of a list of every single artifact published into the repository. By analyzing
the names of artifacts and groups and checking to see if those names could be
expressed using the restricted dotted name specification here, we observed
the following:

  * There are `69604` unique group names on Maven Central. Of these,
    `68690` have names that are expressible using the syntax defined
    here. This leaves `914` inexpressible groups, for a coverage of
    `98.69%`.

  * There are `431423` unique artifact names on Maven Central. Of these,
    `343678` have names that are expressible using the syntax defined
    here. This leaves `87745` inexpressible names, for a coverage of
    `79.66%`.

However, we also analyzed the reasons that names failed to match the syntax
defined here and determined:

  * `618` and `24810` group and artifact names, respectively, failed to
    match because they contained uppercase characters. If all names are
    converted to lowercase, this removes a significant chunk of "bad"
    names.

  * `62696` artifact names failed to match because they contained characters
    other than `[a-z]` after a dot. These were frequently artifacts that,
    for whatever reason, decided to encode version numbers within the name
    itself. A random sample of failing names is as follows:

    ```
      com.github.javawithmarcus.wicket-cdi-1.1
      org.floggy.3rd.org.eclipse.core
      com.github.1137095129
      io.github.2gis
      com.github.9215095360
      com.9isuper.eve
      org.99soft
      com.moz.kiji.delegation.kiji-delegation.3.0.0.com.moz.kiji.schema
      org.floggy.3rd.org.eclipse.ui
      io.7mind.izumi.sbt
      opentelemetry-armeria-1.0
      common-util_2.13
      mongoauth_3.1_2.12
      content-api-client_2.12
      utils-test_2.12
      kafkakit_2.13
      ciris-refined_2.11
      dynamo-test_2.13
      case-service_2.12
      jimcy-java-api_2.11
    ```

  * Only one single artifact name used a non-ASCII character on Maven
    Central: `com.github.marcioos:bgg-clienẗ`

  * Only `69` artifacts contained name segments that were too long to
    be supported by the syntax defined here.
    A random sample of failing names is as follows:

    ```
    rapidpm-proxybuilder-modules-dynamicobjectadapter-generator-processors
    rapidpm-proxybuilder-modules-objectadapter-generator-usages-usinggenerated
    spring-cloud-starter-stream-processor-tasklaunchrequest-transform
    stormpath-sdk-examples-spring-security-spring-boot-webmvc-bare-bones
    camel-quarkus-integration-tests-support-custom-type-converter-deployment
    wildfly-microprofile-reactive-streams-operators-cdi-provider-legacy-namespace
    camel-quarkus-integration-tests-support-custom-type-converter-parent
    stormpath-sdk-tutorials-spring-boot-default-spring-security-refined
    nav-virksomhet-tiltakOgAktiviteterForBrukere-v1-meldingsdefinisjon
    camel-quarkus-integration-test-support-core-main-collector-ext-deployment
    ```

    In the author's opinion, these names are somewhat excessive and could
    be supported with dotted notation instead of relentless hyphenation.

  * Less than `20` artifacts had any other characters that do not appear in
    the regular expression defined here. A random sample is as follows:

    ```
    # These contain ':'
    libaums:storageprovider
    com.foilen:database-tools
    libaums:http
    reactivex:rxjs
    app.ubie:brave-kt

    # These contain '+'
    bctsp-jdk15+
    bcpg-jdk15+
    sugar-tms_2.12at13+
    mvp+android
    bcprov-jdk15+
    amiitool+android
    bcmail-jdk15+

    # These contain whitespace
    com.inkapplications.spondee.math-macosx64.0.0.3.com 2.inkapplications.spondee
    com.inkapplications.spondee.math-macosx64.0.0.3.com 3.inkapplications.spondee
    com.inkapplications.spondee.math-macosx64.0.0.3.com 4.inkapplications.spondee
    utilex

    # These contain quote characters
    "palsolayouts"
    "android-sdk"
    "rxbluetooth"
    ```

    Many of these look like publication mistakes.

It is therefore the position of the author that if people are publishing packages
with non-English names, they appear to be doing it using the ASCII character
set.

_Why are the length of names bounded?_

For two reasons:

  * Regular expressions can be subject to [denial of service](https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS)
    attacks, particularly when they contain unbounded quantifiers such as `*`
    and `+`.

  * Adding an upper bound on length means more predictable storage use when
    names are used in relational databases.

The regular expression as it is defined here is expected to be somewhat
less vulnerable to denial of service attacks in naive regex engine
implementations than an unbounded version would be.

