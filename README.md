Before using ostrich.., you need to:
install **sbt 1.2.1** on linux

Run ostrich
-------------------------
simply run follow command :

```
$ cd fold of ostrich
$ sbt "run tests/hu-benchmarks/indexof-1"
```

Get executable jar, located in target/scala-2.11/ostrich-assembly-1.0.jar
-------------------------------------------------------------------------

`$ sbt assembly`

Run the jar with arguement
--------------------------

`$ java -jar  ostrich-assembly-1.0.jar [-timeout=num] [-nuxmvtime=num] [-useparikh] [-strategy=] testfile`

(have two **strategy** argument: *-I* and *-F*. *-I* use complete algorithm while *-F* use heuristic algorithm. *-F* is faster)

or (***recommanded***)

`$ python script.py testfile`
