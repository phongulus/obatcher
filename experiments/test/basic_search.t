  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe add btree.bc 5 "key 5"
  $ ./btree_tester.exe add btree.bc 4 "key 4"
  $ ./btree_tester.exe add btree.bc 6 "key 6"

  $ ./btree_tester.exe search btree.bc 3
  None
  $ ./btree_tester.exe search btree.bc 4
  "key 4"
  $ ./btree_tester.exe search btree.bc 5
  "key 5"
  $ ./btree_tester.exe search btree.bc 6
  "key 6"
  $ ./btree_tester.exe search btree.bc 7
  None
