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



Example
-------

The following information can be retrieved from Elasticsearch

~~~~{.json}
{
  "took" : 1,
  "timed_out" : false,
  "_shards" : {
    "total" : 5,
    "successful" : 5,
    "failed" : 0
  },
  "hits" : {
    "total" : 1,
    "max_score" : 1.0,
    "hits" : [
      {
        "_index" : "rsyslog-test",
        "_type" : "%{@type}",
        "_id" : "AVwWnWEqQ4ADFiA38GPp",
        "_score" : 1.0,
        "_source" : {
          "@timestamp" : "2017-05-17T13:33:51.690Z",
          "port" : 58031,
          "@version" : "1",
          "host" : "0:0:0:0:0:0:0:1",
          "torque" : {
            "owner" : "someuser@login.mycluster.mydomain.com",
            "startCount" : null,
            "resourceUsage" : {
              "mem" : 66027520,
              "vmem" : 322555904,
              "cputime" : 23076,
              "walltime" : {
                "hours" : 6,
                "seconds" : 15,
                "minutes" : 25,
                "days" : 0
              },
              "energy" : 0
            },
            "resourceRequest" : {
              "neednodes" : {
                "Right" : [
                  {
                    "name" : "mynode",
                    "ppn" : 1
                  }
                ]
              },
              "nodes" : {
                "Right" : [
                  {
                    "name" : "mynode",
                    "ppn" : 1
                  }
                ]
              },
              "vmem" : 4720302336,
              "nodeCount" : 1,
              "walltime" : {
                "hours" : 71,
                "seconds" : 59,
                "minutes" : 59,
                "days" : 0
              },
              "nice" : 0
            },
            "session" : 102034,
            "times" : {
              "qtime" : 1494689613,
              "etime" : 1494689613,
              "ctime" : 1494689613,
              "startTime" : 1494689684,
              "endTime" : 1494712802
            },
            "totalExecutionSlots" : 1,
            "name" : {
              "cluster" : "mycluster",
              "number" : 3275189,
              "array_id" : null,
              "master" : "master"
            },
            "uniqueNodeCount" : 1,
            "user" : "someuser",
            "exitStatus" : 0,
            "jobname" : "myjob",
            "queue" : "long",
            "group" : "somegroup"
          },
          "message" : "05/14/2017 00:00:02;E;3275189.master.mycluster.mydomain.com;user=someuser group=somegroup jobname=myjob queue=long ctime=1494689613 qtime=1494689613 etime=1494689613 start=1494689684 owner=someuser@login.mycluster.mydomain.com exec_host=mynode.mycluster.mydomain.com/1 Resource_List.neednodes=mynode:ppn=1 Resource_List.nice=0 Resource_List.nodect=1 Resource_List.nodes=mynode:ppn=1 Resource_List.vmem=4720302336b Resource_List.walltime=71:59:59 session=102034 total_execution_slots=1 unique_node_count=1 end=1494712802 Exit_status=0 resources_used.cput=23076 resources_used.energy_used=0 resources_used.mem=64480kb resources_used.vmem=314996kb resources_used.walltime=06:25:15",
          "syslog_abspri" : "5",
          "type" : "normalised_syslog",
          "tags" : [ ]
        }
      }
    ]
  }
}
~~~~
