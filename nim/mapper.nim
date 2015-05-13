from sequtils import toSeq, mapIt
import strutils
import os
import threadpool
import tables
import re

iterator filter(filename: string): string = 
    let KNICKS=re("knicks", flags={reIgnoreCase}) 
    var parts: seq[string]
    for line in lines(filename):
        parts = line.split('\t')
        if parts[3].contains(KNICKS):
            yield parts[1]

proc mapper(filename: string): seq[string] = 
    return toSeq(filter(filename))

proc reduce(filteredfiles: seq[seq[string]]): CountTable[string] = 
    result = initCountTable[string]()    
    for filtered in filteredfiles:
        for line in filtered:
            result.inc(line)
    result.sort()

var files = mapIt(toSeq(walkDir("tmp/tweets")), string, it[1]) # pull a seq of the files containing tweets
var results = files.mapIt(FlowVar[seq[string]], spawn mapper(it)) # apply mapper across multiple threads
var total = reduce(results.mapIt(seq[string], ^it)) # wait for each thread to finish

var output = open("tmp/nim_output", fmWrite)
try:
    for key, value in total.pairs():
        output.writeln(key & '\t' & $value)
finally:
    close(output)
