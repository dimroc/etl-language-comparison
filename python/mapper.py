""" Python take on https://github.com/dimroc/etl-language-comparison
"""
import re
import os
from operator import itemgetter
from itertools import *
from multiprocessing import Pool
from multiprocessing import cpu_count

KNICKS = re.compile(r"knicks", re.I)


def mapper(filename):
    '''
    Given a file, map a list of tuples
    as (neighborhood, 1) if the message contains 'knicks'
    '''

    tweetfile = open(filename,'r')
    #helper closure to parse the line into a tuple
    #(int, neighborhood, city, message)
    def get_parsed_tweet():
        return (x.split('\t') for x in tweetfile)

    # create a list of tuples in the form
    # (neighborhood, 1)
    # if there is a mention of knicks
    res = [
           (x[1], 1)
           for x in get_parsed_tweet()
           if KNICKS.search(x[3])
          ]

    tweetfile.close()
    return res


def reduce(results):
    data = sorted(results, key=itemgetter(0))
    # k will be the town
    # g will be a list of (town, count)
    for k, g in groupby(data, itemgetter(0)):
        yield (k, sum([x[1] for x in g]))


def run_file(filename):
    data = mapper(filename)
    tuples =  list(reduce(data))
    #print("{} produced {} sets".format(filename, len(tuples)))
    return tuples


if __name__ == '__main__':
    with Pool(cpu_count()) as p:
        #use multiprocessing to run one file per cpu
        map_reduced_per_file = p.map(run_file, ["../tmp/tweets/" + x for x in os.listdir('../tmp/tweets/') if x.startswith('tweets_')])

        #for each file we have a list of tuples. chain will 'flatmap' them for another
        #reduce pass
        final_results =  list(reduce(chain.from_iterable(map_reduced_per_file)))

        #sort by size and print for ease of reading
        final_results = sorted(final_results, key=itemgetter(1), reverse=True)
        with open("../tmp/python_output", 'w+') as o:
            o.writelines(("{}\t{}\n".format(res[0], res[1]) for res in final_results))
