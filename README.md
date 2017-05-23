hNormalise
==========

`hNormalise` is a small tool and accompanying library for the conversion of regular [rsyslog](http://www.rsyslog.com) to
structured log messages, i.e., turning the `msg` payload into (a nested) JSON object.

Features:

- accepts JSON-style rsyslog data (sent as %jsonmesg% in the rsyslog template)
- accepts legacy encoded rsyslog data (currently sent with the fixed template
  <%PRI%>1 %TIMEGENERATED% %HOSTNAME% %SYSLOGTAG% - %APPNAME%: %msg%)
- sends out successfull converted results on a TCP port, allowing communication back to a wide range of services,
  including rsyslog, [logstash](http://www.elastic.co/products/logstash), ...
- sends out original messages to a (different) TCP port in case the parsing fails, allowing other services to process the
  information.

Usage and configuration
-----------------------

To run and build `hNormalise`, clone this repository and run `stack build` followed by `stack install` inside it.
To start just run `hnormalise`. If you need help, use the `-h` flag. To run the included tests, run `stack test`.
To run the included benchmarks, run `stack bench` (with the `--output target.html` flag to get a nice web page with
the results).

Ports and machines can be tweaked through a configuration file. See `data/hnormalise.yaml` for an example.

Testing the actual setup can be done trivially via `nc`, provided you have data to throw at `hNormalise`. A test example
is also provided below, or you can get useful examples from the tests, under `test/HNormalise/*/ParserSpec.hs`

Parsing
-------

`hNormalise` uses the [Attoparsec](https://github.com/bos/attoparsec) package to have fast and efficient parsing.
`Attoparsec` offers a clean and relatively simple DSL that allows getting the relevant data from the message and
discarding the rest. We also rely on [permute]() to deal with log lines that may contain e.g., key-value pairs in
no definite ordering. Note that this _will_ slow down the parsing.


Caveat: at this point, we do not a priori restrict the possible parsers we unleash on each message. However, if the inbound
data can be tagged properly, we could reduce the maximal number of parsers tried and avoid extensive backtracking.

Supported log messages
----------------------

Currently, hNormalise supports several log messages out of the box (i.e., the ones we need :)
- [Torque](http://www.adaptivecomputing.com/products/open-source/torque/) accounting logs for a job exit.
- [Shorewall](http://shorewall.org) firewall log messages for TCP, UDP and ICMP connections
- [Lmod](https://www.tacc.utexas.edu/research-development/tacc-projects/lmod) module load messages

More are forthcoming soon, e.g., (in no particular order)
- GPFS
- Icinga
- NFS
- OpenNebula
- SSH
- Snoopy
- Jube

### Adding a new parser

To add a new parser for log lines from tool `Tool`, several actions must be taken:

- A `src/HNormalise/Tool` directory must be created, under which all specific code and data types for the new
  log lines will reside. Note that `Tool` can provide multiple types of log lines, but they should all be coded under
  the same location.
- Define a data type that holds the relevant data from the log line you wish to keep/forward in `HNOrmalise.Tool.Internal`.
  Make sure that the type derives `Generic` (and add the require language extentions on top of the file).
- The parser goes in the `HNormalise.Tool.Parser` module.
- Conversion of the data type that was defined to hold the data to JSON is done in `HNormalise.Tool.JSON`.
- Finally, the `HNormalise` module defines a sum-type container to which you should add your own entry. Remember to also
  add a line for the corresponding getJsonKey function, which defines the key under whoch the parsed data will be
  made available downstream.
- Add tests with relevant cases under `test/HNormalise/Tool/ParserSpec.hs`. The framework should pick these up
automagically.
- Add en entry for your test cases to the set of benchmarks, so we can have a reasonable estimate on how your parser is
  performing. Update the HTML page under `benchmarks` with the complete set of tests you ran.


Example
-------

The original (anonymised) message sent by rsyslog (as JSON) for a Torque job exit event is

~~~~
{"msg":"05/14/2017 00:00:02;E;3275189.master.mycluster.mydomain.com;user=someuser group=somegroup jobname=myjob queue=long ctime=1494689613 qtime=1494689613 etime=1494689613 start=1494689684 owner=someuser@login.mycluster.mydomain.com exec_host=mynode.mycluster.mydomain.com/1 Resource_List.neednodes=mynode:ppn=1 Resource_List.nice=0 Resource_List.nodect=1 Resource_List.nodes=mynode:ppn=1 Resource_List.vmem=4720302336b Resource_List.walltime=71:59:59 session=102034 total_execution_slots=1 unique_node_count=1 end=1494712802 Exit_status=0 resources_used.cput=23076 resources_used.energy_used=0 resources_used.mem=64480kb resources_used.vmem=314996kb resources_used.walltime=06:25:15", "rawmsg": "redacted", "timereported": "2017-05-15T18:16:16.724002+02:00", "hostname": "master.mydomain.com", "syslogtag": "hnormalise", "inputname": "imfile", "fromhost": "", "fromhost-ip": "", "pri": "133", "syslogfacility": "16", "syslogseverity": "5", "timegenerated": "2017-05-15T18:16:16.724002+02:00", "programname": "hnormalise", "protocol-version": "0", "structured-data": "-", "app-name": "hnormalise", "procid": "-", "msgid": "-", "uuid": null, "$!": null }
~~~~

The resulting JSON is sent to logstash, which forwarded it to ES, e.g. with the following configuration

~~~~
input {
    tcp {
        type => "normalised_syslog"
        port => 26002
        codec => "json"
    }
}

filter {
    if [type] == 'normalised_syslog' {
        mutate {
            add_field => {
                "[@metadata][target_index]" => "rsyslog-test"
            }
        }
    }
}

output {
    elasticsearch {
        template_overwrite => true
        document_type => "%{@type}"
        index => "%{[@metadata][target_index]}"
        hosts => [ "127.0.0.1" ]
        flush_size => 50
	}
}
~~~~

The following information can be retrieved from Elasticsearch. The interesting part is the `torque` entry, which is what we
aimed to get.

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
