# Important

Our VM contains an evaluation license of [Sicstus](https://sicstus.sics.se/)
which is valid until July 28th. If you planning to evaluate this artifact later
than that, please contact us for instructions on how to renew the license.

#  Getting Started Guide

- **VM player**               : VirtualBox 5.1.22
- **VM player download link** : https://www.virtualbox.org/wiki/Linux_Downloads
- **VM username**             : paper34
- **VM password**             : paper34

## Reproducing the results

To reproduce our results:

1. Simply double click the terminal icon on the desktop, or open a terminal and
   `cd` into `~/Desktop/artifact/brisk-benchmarks`.
2. Run `./test`

`test` will simply iterate over all the benchmarks and run `brisk` on them. The
output will present safe and unsafe programs in two sets. The first column
signifies whether the Brisk was able to sequentialize the given program or not.
Second column will print the name of the benchmark, which is also the name of
the directory. Third column is the amount it takes to rewrite the program. It
_does not_ include the extra overhead of starting `stack`, parsing the file,
etc. that we do not consider. Last column contains the total time that contains
all the overheads.

## Sample Output

```
---- Positive Benchmarks -------------------------
          Benchmark Name         Rewrite   Total
--------------------------------------------------
[SUCCESS] MultiPing                30 ms   4400 ms
[SUCCESS] AsyncP                   30 ms   4400 ms
[SUCCESS] PingDet                  30 ms   4600 ms
[SUCCESS] PingIter                 30 ms   4700 ms
[SUCCESS] PingSym                  20 ms   4600 ms
[SUCCESS] PingSym2                 30 ms   4600 ms
[SUCCESS] ConcDB                   20 ms   4500 ms
[SUCCESS] DistDB                   10 ms   4600 ms
[SUCCESS] Firewall                 40 ms   4600 ms
[SUCCESS] LockServer               30 ms   4500 ms
[SUCCESS] MapReduce                30 ms   4700 ms
[SUCCESS] Parikh                   20 ms   4500 ms
[SUCCESS] Registry                 30 ms   4900 ms
[SUCCESS] TwoBuyers                30 ms   4600 ms
[SUCCESS] TwoPhaseCommit           40 ms   4800 ms
[SUCCESS] WorkSteal                60 ms   4700 ms

---- Negative Benchmarks -------------------------
          Benchmark Name         Rewrite   Total
--------------------------------------------------
[ERROR]   AsyncPWrongType          20 ms   4700 ms
[ERROR]   AsyncPWithRace           10 ms   4600 ms
[ERROR]   MapReduceNoWork        1010 ms   6000 ms
[ERROR]   MapReduceNoTerm          30 ms   4600 ms
[ERROR]   MapReduceCF            1020 ms   5500 ms
[ERROR]   MapReduceNoMaster        50 ms   4500 ms
[ERROR]   MapReduceNoReduce        40 ms   4700 ms
[ERROR]   FirewallWrongPid         40 ms   4800 ms
[ERROR]   WorkStealCF              20 ms   4400 ms
```

## Running a single benchmark

If you want to run an individual benchmark you can do the following:

1. Simply double click the terminal icon on the desktop, or open a terminal and
   `cd` into `~/Desktop/artifact/brisk-benchmarks`.
2. Run `stack exec -- brisk --file <file path> --binder <function name>`.

Each benchmark is split into different modules that contain the specification of
each symmetric processes. In general, a master process will initialize the
system by spawning the rest of the processes. The file that contains the master
process, and the specific function that handles it is specified when `brisk` is
run. To see the list of file and function name pairs that `test` script uses,
please take a look at `brisk-benchmarks/src/Main.hs` starting at line 39.

For example, if you want to run `ConcDB` only, you can do the following:

```
$ cd ~/Desktop/artifact/brisk-benchmarks
$ stack exec -- brisk --file src/ConcDB/Database.hs --binder database
Compiling...
[1 of 1] Compiling ConcDB.Database  ( src/ConcDB/Database.hs, interpreted )
Loading...
sicstus -l /home/rami/work/oopsla17-artifact/brisk-benchmarks/.stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/share/x86_64-linux-ghc-8.0.2/brisk-0.0.0.2/rewrite.pl --goal "consult('./brisk_query1804289383846930886'),rewrite_query(T,R),(catch(check_race_freedom(T,T1),_,halt(3)),!, (catch(rewrite(T1,R,D,_),_,halt(4)); halt(4)) ; halt(3)),set_output(user_output),portray_clause(D),halt(0)." --noinfo --nologo 2>/dev/null 
rewrite in: 20ms
seq([assign(a,loop1,1),for(A,b_Set,[assign(A,loop0,1)])]).
OK
```

The output will contain:

- The we use to call `sicstus` in order to rewrite the the program
- Runtime of the rewrite
- Sequentialized version of the program (in Sicstus)

# Step-by-Step Instructions

Our artifact contains 3 folders inside `~/Desktop/artifact/`:

## brisk-prelude

...

## brisk

...

## brisk-benchmarks

This folder contains the benchmarks we have used to evaluate our tool. Each
folder in `brisk-benchmarks/src` contains a single benchmark.

