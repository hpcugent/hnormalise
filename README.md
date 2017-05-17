hNormalise
==========

`hNormalise` is a small tool and accompanying library for the conversion of regular [rsyslog](http://www.rsyslog.com) to
structured log messages, i.e., turning the `msg` payload into (a nested) JSON object.

Features:

- accepts JSON-style rsyslog data (sent as %jsonmesg% in the rsyslog template)
- sends out successfull converted results on a TCP port, allowing communication back to a wide range of services,
  including rsyslog, [logstash](http://www.elastic.co/products/logstash), ...
- sends out original messages to a TCP port n case the parsing failed, allowing other services to process the
  information.


Parsing
-------

`hNOrmalise` used the [Attoparsec](https://github.com/bos/attoparsec) package to have fast and efficient parsing.
`Attoparsec` offers a clean and relatively simple DSL that allows getting the relevant data from the message and
discarding the rest. We also rely on [permute]() to deal with log lines that may contain e.g., key-value pairs in
no definite ordering. Note that this _will_ slow down the parsing.


### Adding a new parser

To add a new parser for logs from tool `Tool`, you should create a `src/HNormalise/Tool` directory.
The parser goes in `HNormalise.Tool.Parser`. The corresponding Haskell data structure goes in `HNOrmalise.Tool.Internal`.

The conversion of the parser to JSON is defined in `HNormalise.Tool.JSON`.

Next to this, you should add an element to the `ParseResult` ADT in the `HNormalise` module and let getJsonKey know
what the key should be for this added element.
