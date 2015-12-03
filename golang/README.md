## Nov 23rd 2015 - @matttproud

This commit cleans up a few things about the Go pipeline:

1. idiom: use sync.WaitGroup instead of atomic counter for
rendezvous.

2. throughput: switch to buffered channels since message handout
needn't be synchronous.

3. cruft removal: removed dead/non-used functions.

4. throughput: kick off reading from all buffers in separate Go
routines since the scheduler will self-bound their operation
per the maximum processors.

5. simplify/throughput: drop pre-processing of tallies into maps
and replace it with a channel that merely contains the
neighborhood name. Neighborhood processing occurs in-flight
with the mappers.

6. throughput: the regexp/substring match can be made more
efficient by prima facie rejecting candidates whose values are
too short to contain the needle.

7. readability: separate mapper and reducers into individual
routines.

8. tidying: remedy lint errors.

Overall this change is performance neutral to being an improvement.
In my local runs, I was able to shave off about 2.5 seconds on the
substring run with this.
