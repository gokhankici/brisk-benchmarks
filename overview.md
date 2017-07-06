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
[SUCCESS] ThequeFS                 80 ms   4600 ms

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

Each folder in `~/Desktop/artifact/brisk-benchmarks/src` contains a single
benchmark, and each benchmark in is split into different modules that contain
the specification of each symmetric processes. Generally, a master process will
initialize the system by spawning the rest of the processes. The file that
contains the master process, and the specific function that handles it is
specified when `brisk` is run. To see the list of file and function name pairs
that `test` script uses, please take a look at `brisk-benchmarks/src/Main.hs`
starting at line 39.

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

## ThequeFS

To run `ThequeFS`, please do the following:

1. Open a terminal and `cd` into `/home/paper34/Desktop/artifact/thequefs` 
2. Run `stack exec -- brisk --file app/Scenario.hs --binder main`

Sample output:

```
Compiling...
[1 of 1] Compiling ThequeScenario   ( app/Scenario.hs, interpreted )
Skipping binding group!
Skipping binding group!
Loading...
sicstus -l /home/paper34/Desktop/artifact/thequefs/.stack-work/install/x86_64-linux/lts-8.3/8.0.2/share/x86_64-linux-ghc-8.0.2/brisk-0.0.0.2/rewrite.pl --goal "consult('./brisk_query1804289383846930886'),rewrite_query(T,R),(catch(check_race_freedom(T,T1),_,halt(3)),!, (catch(rewrite(T1,R,D,_),_,halt(4)); halt(4)) ; halt(3)),set_output(user_output),portray_clause(D),halt(0)." --noinfo --nologo 2>/dev/null 
rewrite in: 80ms
seq([assign(a,anf0,cstr__MS(c_Set,d_Set,ds_dxYm)),assign(a,dll__dll__brisk_anf_dyab,anf0),assign(a,st0,dll__dll__brisk_anf_dyab),assign(a,loop1,1),for(A,c_Set,[assign(A,anf0,cstr__DNS(dll__dll__brisk_anf_dtNw))]),for(A,c_Set,[assign(A,st0,anf0)]),for(A,c_Set,[assign(A,loop0,1)]),for(B,b_Set,[[assign(a,loop1,0),cases(B,ndet,[case(B,cstr__Tuple(cmd,dat,name,refs),[assign(B,ndet,cstr__Tuple(cmd,dat,name,refs)),cases(B,dll__dll__brisk_anf_d2r,[case(B,cstr__Get(null___),[assign(B,dll__dll__brisk_anf_d2r,cstr__Get(null___)),assign(B,anf1,cstr__TagId(name)),assign(B,anf0,cstr__GetTag(anf1)),assign(B,rpc,anf0),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf21,cstr__Just(ndet)),assign(a,best,anf21)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf22,cstr__Just(ndet)),assign(a,best,anf22)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])]])]),cases(a,res,[[assign(a,res,cstr__Nothing(null___)),assign(B,dll__dll__brisk_anf_d2r,cstr__Get(null___)),assign(B,anf1,cstr__TagId(name)),assign(B,anf0,cstr__GetTag(anf1)),assign(B,rpc,anf0),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf21,cstr__Just(ndet)),assign(a,best,anf21)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf22,cstr__Just(ndet)),assign(a,best,anf22)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])]])]),assign(a,anf23,cstr__Nil(null___))],[assign(a,res,cstr__Just(zl)),assign(B,dll__dll__brisk_anf_d2r,cstr__Get(null___)),assign(B,anf1,cstr__TagId(name)),assign(B,anf0,cstr__GetTag(anf1)),assign(B,rpc,anf0),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf21,cstr__Just(ndet)),assign(a,best,anf21)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]]),assign(a,anf22,cstr__Just(ndet)),assign(a,best,anf22)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf20,cstr__GetTag(tid)),assign(a,anf19,cstr__Tuple(a,anf20)),assign(a,askedTag,pay)]])]])])]]),assign(a,dll__dll__brisk_anf_dy9Y,anf23),assign(a,anf25,cstr__TagRefs(dll__dll__brisk_anf_dy9Y)),assign(a,anf24,cstr__ReplyMsg(anf25,st0)),assign(a,reply,anf24),assign(a,anf34,cstr__SelfSigned(a,r)),assign(a,st0,s_),assign(a,loop1,1)]),case(B,cstr__Add(null___),[assign(B,dll__dll__brisk_anf_d2r,cstr__Add(null___)),assign(B,anf3,cstr__TagId(name)),assign(B,anf2,cstr__AddTag(anf3,refs)),assign(B,rpc,anf2),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf28,cstr__Just(ndet)),assign(a,best,anf28)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf29,cstr__Just(ndet)),assign(a,best,anf29)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])]])]),cases(a,best,[[assign(a,best,cstr__Nothing(null___)),assign(B,dll__dll__brisk_anf_d2r,cstr__Add(null___)),assign(B,anf3,cstr__TagId(name)),assign(B,anf2,cstr__AddTag(anf3,refs)),assign(B,rpc,anf2),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf28,cstr__Just(ndet)),assign(a,best,anf28)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf29,cstr__Just(ndet)),assign(a,best,anf29)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])]])]),assign(a,anf30,cstr__Tag(addtag,refs,0))],[assign(a,best,cstr__Just(zp)),assign(B,dll__dll__brisk_anf_d2r,cstr__Add(null___)),assign(B,anf3,cstr__TagId(name)),assign(B,anf2,cstr__AddTag(anf3,refs)),assign(B,rpc,anf2),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,best,cstr__Nothing(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,askedTag,[[assign(a,askedTag,cstr__OK(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagInfo(ds_dxWS)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,askedTag,cstr__TagFound(tf)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,tf,[[assign(a,tf,cstr__Tag(ds_dxWP,ds_dxWQ,ds_dxWR)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy91,[[assign(a,dll__dll__brisk_anf_dy91,cstr__Nothing(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf28,cstr__Just(ndet)),assign(a,best,anf28)],[assign(a,dll__dll__brisk_anf_dy91,cstr__Just(idx)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),cases(a,dll__dll__brisk_anf_dy94,[[assign(a,dll__dll__brisk_anf_dy94,cstr__False(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])],[assign(a,dll__dll__brisk_anf_dy94,cstr__True(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]]),assign(a,anf29,cstr__Just(ndet)),assign(a,best,anf29)]])]])]])],[assign(a,askedTag,cstr__TagNotFound(null___)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf27,cstr__GetTag(addtag)),assign(a,anf26,cstr__Tuple(a,anf27)),assign(a,askedTag,pay)]])]])]),assign(a,anf30,cases(a,zp,[case(a,cstr__Tag(ds_dxX1,ds_dxX2,ds_dxX3),cstr__Tag(ds_dxX1,ndet,ndet))],skip))]]),assign(a,dll__dll__brisk_anf_dy9l,anf30),assign(a,ds_dxXb,cstr__Unit(null___)),for(C,d_Set,[assign(a,tn,C),assign(a,anf32,cstr__SetTag(addtag,dll__dll__brisk_anf_dy9l)),assign(a,anf31,cstr__Tuple(a,anf32)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,tn,C),assign(a,anf32,cstr__SetTag(addtag,dll__dll__brisk_anf_dy9l)),assign(a,anf31,cstr__Tuple(a,anf32)),assign(a,v_a,pay)]])]),assign(a,anf33,cstr__ReplyMsg(cstr__OK(null___),st0)),assign(a,reply,anf33),assign(a,anf34,cstr__SelfSigned(a,r)),assign(a,st0,s_),assign(a,loop1,1)]),case(B,cstr__Alloc(null___),[assign(B,dll__dll__brisk_anf_d2r,cstr__Alloc(null___)),assign(B,anf4,cstr__AddBlob(name,3)),assign(B,rpc,anf4),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,anf2,cstr__MS(ds_dxWa,ds_dxWb,ndet)),assign(a,anf1,cstr__Tuple(ndet,anf2)),assign(a,ds_dxXH,anf1),assign(a,anf3,blob),assign(a,blob,anf3),assign(a,anf4,s_),assign(a,s_,anf4),assign(a,dll__dll__brisk_anf_dy9u,blob),assign(a,anf5,ds_dxZ7),assign(a,srvs,anf5),assign(a,dll__dll__brisk_anf_dy9v,ndet),assign(a,dll__dll__brisk_anf_dy9y,s_),assign(a,anf7,cstr__AddBlobServers(dll__dll__brisk_anf_dy9u,dll__dll__brisk_anf_dy9v)),assign(a,anf6,cstr__ReplyMsg(anf7,dll__dll__brisk_anf_dy9y)),assign(a,reply,anf6),assign(a,anf34,cstr__SelfSigned(a,r)),assign(a,st0,s_),assign(a,loop1,1)]),case(B,cstr__Push(null___),[assign(B,dll__dll__brisk_anf_d2r,cstr__Push(null___)),assign(B,anf5,cstr__PushBlob(name,dat)),assign(B,rpc,anf5),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,ds_dxXf,cstr__Unit(null___)),for(D,c_Set,[assign(a,dn,D),assign(a,anf9,cstr__AddBlob(bid,dat)),assign(a,anf8,cstr__Tuple(a,anf9)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,dn,D),assign(a,anf9,cstr__AddBlob(bid,dat)),assign(a,anf8,cstr__Tuple(a,anf9)),assign(a,v_i,pay)]])]),assign(a,anf10,cstr__ReplyMsg(cstr__OK(null___),st0)),assign(a,reply,anf10),assign(a,anf34,cstr__SelfSigned(a,r)),assign(a,st0,s_),assign(a,loop1,1)]),case(B,cstr__CGetBlob(null___),[assign(B,dll__dll__brisk_anf_d2r,cstr__CGetBlob(null___)),assign(B,anf6,cstr__GetBlob(name)),assign(B,rpc,anf6),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,r,cstr__Nothing(null___)),for(D,c_Set,[assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),assign(a,b,pay)]]),assign(a,anf13,cstr__Just(b)),assign(a,dll__dll__brisk_anf_dy9J,anf13)]),cases(a,b,[[assign(a,b,cstr__Nothing(null___)),assign(B,dll__dll__brisk_anf_d2r,cstr__CGetBlob(null___)),assign(B,anf6,cstr__GetBlob(name)),assign(B,rpc,anf6),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,r,cstr__Nothing(null___)),for(D,c_Set,[assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),assign(a,b,pay)]]),assign(a,anf13,cstr__Just(b)),assign(a,dll__dll__brisk_anf_dy9J,anf13)]),assign(a,anf15,cstr__DNResponse(cstr__BlobNotFound(null___))),assign(a,anf14,cstr__ReplyMsg(anf15,st0)),assign(a,v_r,anf14)],[assign(a,b,cstr__Just(dn)),assign(B,dll__dll__brisk_anf_d2r,cstr__CGetBlob(null___)),assign(B,anf6,cstr__GetBlob(name)),assign(B,rpc,anf6),assign(B,anf7,cstr__Tuple(B,rpc)),assign(a,ds_deMy,anf7),assign(a,r,cstr__Nothing(null___)),for(D,c_Set,[assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),cases(a,le_msg,[[assign(a,le_msg,cstr__SelfSigned(a,pay)),assign(a,dn,D),assign(a,anf12,cstr__GetBlob(bid)),assign(a,anf11,cstr__Tuple(a,anf12)),assign(a,b,pay)]]),assign(a,anf13,cstr__Just(b)),assign(a,dll__dll__brisk_anf_dy9J,anf13)]),assign(a,anf17,cstr__DNResponse(dn)),assign(a,anf16,cstr__ReplyMsg(anf17,st0)),assign(a,v_r,anf16)]]),assign(a,anf18,cstr__ReplyMsg(cstr__OK(null___),st0)),assign(a,reply,anf18),assign(a,anf34,cstr__SelfSigned(a,r)),assign(a,st0,s_),assign(a,loop1,1)])])])]),assign(B,le_msg,anf34)]]),for(B,b_Set,[assign(B,null,pay)])]).
OK
```

# Included Directory

Our artifact contains 4 directories inside `~/Desktop/artifact/`:

## brisk

The `brisk` program and library is the core of the implementation of our
analysis.
`brisk` is implemented as a GHC plugin pass that computes a first-order term
corresponding to the binders in a given module, and _stores_ this information
in a table in the final, compiled Haskell module.
The package includes specifications for some of the built-in message passing
primitives as well as the specifications for the `Monad` typeclass instance for `Process`

This package also includes our Prolog `IceT` rewriter.

## brisk-prelude

The `brisk-prelude` library includes `GHC` annotations for some common Haskell
routines.
The modules exported by `brisk-prelude` can be imported to bring the `brisk`
annotations into scope: these annotations can then be used by the `brisk` tool.

For example, `src/GHC/Base/Brisk.hs` includes annotations that specify
the behavior of common functions like `($)` and `maybe`.

...

## thequefs

This directory contains the `thequefs` implementation. To create a "closed
world," we ran `brisk` on `app/Scenario.hs`, which spawns a number of clients who interact
with the filesystem.

...

## brisk-benchmarks

This directory simply contains the benchmarks we have used to evaluate our tool.
Each directory in `brisk-benchmarks/src` contains a single benchmark.

