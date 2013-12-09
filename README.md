GT4IREval
=========

An R package to measure the reliability of an Information Retrieval test collection with Generalizability Theory.

For a full background please refer to *Julián Urbano, Mónica Marrero, and Diego Martín, "[On the Measurement of Test Collection Reliability](http://julian-urbano.info/wp-content/uploads/055-measurement-test-collection-reliability.pdf)", International ACM SIGIR Conference on Research and Development in Information Retrieval, 2013*.

Usage
-----

A full user manual in PDF is available from the [releases page](https://github.com/julian-urbano/GT4IREval/releases).

As a very simple example, you can load your data from a file such as ``adhoc3.txt``:

    > ah3 <- read.table("adhoc3.txt")
    > head(ah3)
    sys1 sys2 sys3 sys4 sys5 sys6 sys7 ...
    1 0.2830 0.5163 0.4810 0.5737 0.5184 0.4945 0.5013 ...
    2 0.0168 0.5442 0.3987 0.2964 0.6115 0.2354 0.1689 ...

Then run a G-study:

    > ah3.g <- g.study(ah3, drop = 0.25)
    > ah3.g
    
    Summary of G-Study
    
                     Systems     Queries Interaction
                 ----------- ----------- -----------
    Variance       0.0028117    0.028093    0.010152
    Variance(%)       6.8482      68.425      24.727
    ---
    Mean Sq.         0.15074     0.85296    0.010152
    Sample size           30          50        1500

and a D-study:

    > d.study(ah3.g)
    
    Summary of D-Study
    
    Call:
        queries = 50 
      stability = 0.95 
          alpha = 0.025 
    
    Stability:
                                               Erho2                                   Phi
                 -----------------------------------   -----------------------------------
         Queries    Expected       Lower       Upper      Expected       Lower       Upper
     ----------- ----------- ----------- -----------   ----------- ----------- -----------
              50     0.93265     0.89311     0.96287       0.78613     0.66141     0.88039 
    
    Required number of queries:
                                               Erho2                                   Phi
                 -----------------------------------   -----------------------------------
       Stability    Expected       Lower       Upper      Expected       Lower       Upper
     ----------- ----------- ----------- -----------   ----------- ----------- -----------
            0.95          69          37         114           259         130         487
