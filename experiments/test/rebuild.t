  $ ./btree_tester.exe init btree.bc
  $ ./btree_tester.exe add btree.bc 1 "key 1"
  $ ./btree_tester.exe add btree.bc 14 "key 14"
  $ ./btree_tester.exe print btree.bc
  node(n=2,leaf=true,no_elts=2)
   - values=[1: "key 1"; 14: "key 14"]
  

  $ ./btree_tester.exe rebuild btree.bc 2,3,4,5,6,7,8,9,10,11,12,13
  node(n=5,leaf=false,no_elts=14)
   - values=[3: "key 3"; 6: "key 6"; 9: "key 9"; 12: "key 12"; 14: "key 14"]
   - child(k=3):
      node(n=3,leaf=true,no_elts=3)
       - values=[1: "key 1"; 2: "key 2"; 3: "key 3"]
  
   - child(k=6):
      node(n=3,leaf=true,no_elts=3)
       - values=[4: "key 4"; 5: "key 5"; 6: "key 6"]
  
   - child(k=9):
      node(n=3,leaf=true,no_elts=3)
       - values=[7: "key 7"; 8: "key 8"; 9: "key 9"]
  
   - child(k=12):
      node(n=3,leaf=true,no_elts=3)
       - values=[10: "key 10"; 11: "key 11"; 12: "key 12"]
  
   - child(k=14):
      node(n=2,leaf=true,no_elts=2)
       - values=[13: "key 13"; 14: "key 14"]
  
