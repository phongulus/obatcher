  $ ./skiplist_tester.exe init ./sls 
  $ ./skiplist_tester.exe print ./sls
  Level 0 : Hd -> Null
  
  $ ./skiplist_tester.exe insert ./sls 1
  Level 0 : Hd -> (1) -> Null
  
  $ ./skiplist_tester.exe insert ./sls 2
  Level 0 : Hd -> (1) -> (2) -> Null
  
  $ ./skiplist_tester.exe insert ./sls 3
  Level 0 : Hd -> (1) -> (2) -> (3) -> Null
  

  $ ./skiplist_tester.exe insert ./sls 4
  Level 0 : Hd -> (1) -> (2) -> (3) -> (4) -> Null
  

  $ ./skiplist_tester.exe insert ./sls 5
  Level 0 : Hd -> (1) -> (2) -> (3) -> (4) -> (5) -> Null
  
