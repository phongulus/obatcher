  $ ./btree_tester.exe init_impbatch btree.bc
  node(n=0,leaf=true,no_elts=0)
   - values=[]
  
  $ ./btree_tester.exe imp_insert btree.bc 20
  node(n=1,leaf=false,no_elts=40)
   - values=[12: "key 12"]
   - child(k=12):
      node(n=3,leaf=false,no_elts=17)
       - values=[3: "key 3"; 3: "key 3"; 6: "key 6"]
       - child(k=3):
          node(n=4,leaf=true,no_elts=4)
           - values=[1: "key 1"; 1: "key 1"; 1: "key 1"; 1: "key 1"]
  
       - child(k=3):
          node(n=3,leaf=true,no_elts=3)
           - values=[3: "key 3"; 3: "key 3"; 3: "key 3"]
  
       - child(k=6):
          node(n=4,leaf=true,no_elts=4)
           - values=[3: "key 3"; 4: "key 4"; 4: "key 4"; 6: "key 6"]
  
       - child(k=_):
          node(n=3,leaf=true,no_elts=3)
           - values=[10: "key 10"; 10: "key 10"; 12: "key 12"]
  
   - child(k=_):
      node(n=4,leaf=false,no_elts=22)
       - values=[13: "key 13"; 15: "key 15"; 16: "key 16"; 19: "key 19"]
       - child(k=13):
          node(n=3,leaf=true,no_elts=3)
           - values=[13: "key 13"; 13: "key 13"; 13: "key 13"]
  
       - child(k=15):
          node(n=4,leaf=true,no_elts=4)
           - values=[13: "key 13"; 13: "key 13"; 14: "key 14"; 14: "key 14"]
  
       - child(k=16):
          node(n=4,leaf=true,no_elts=4)
           - values=[15: "key 15"; 15: "key 15"; 15: "key 15"; 16: "key 16"]
  
       - child(k=19):
          node(n=5,leaf=true,no_elts=5)
           - values=[17: "key 17"; 17: "key 17"; 17: "key 17"; 17: "key 17"; 19: "key 19"]
  
       - child(k=_):
          node(n=2,leaf=true,no_elts=2)
           - values=[19: "key 19"; 19: "key 19"]
  

