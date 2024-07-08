from z3 import *


  
## for receiving code to execute that will ask z3
def ask_z3(codeBinary:str):
  # print(f"ask_z3, binary code: {codeBinary}.")
  codeString = codeBinary.decode("utf-8")
  # print(f"ask_z3, executing: {codeString}.")
  local = {}
  exec(codeString, globals(), local)
  result = local['result']==sat
  # print(f"ask_z3, finished: {result}.")
  return result



## below are for tests during development

def private_test_infinite(s):
  e = fpInfinity(FPSort(8,24),False)
  # premies
  t = FP('t',FPSort(8,24))
  # t = Real('t')
  texpr = fpLEQ(t,e)

  n = Int('n')
  # clocks
  x = Real('x')

  # (x>3)
  delta = fpGEQ(fpAdd(RNE(),fpRealToFP(RNE(),x,Float32()),t),3.0)
  s.add(n==3)
  s.add(x==3.0)
  
  return (s, t, delta, texpr)

def private_test_finite(s):
  e = Real('e')

  n = Int('n')
  # clocks
  x = Real('x')

  # premies
  t = Real('t')
  texpr = t<e

  # # (x<3)
  # s.add(e==3)     # e </<= 3
  # delta = x+t<n
  # s.add(n==3)
  # s.add(x==0.0)

  # # (3<x<6)
  # s.add(e==3)     # e </<= 3
  # delta = x+t<n
  # s.add(n==6)
  # s.add(x==3.0)

  # (x=3) # ! does not, just rule out in erlang
  # s.add(e==0.0)     # e </<= 0
  # delta = x+t=n
  # s.add(n==3)
  # s.add(x==0.0)
  
  return (s, t, delta, texpr)

def private_test():
  s = Solver()

  # ## e infinite
  (s, t, delta, texpr) = private_test_infinite(s)

  ## e not infinite
  # (s, delta) = private_test_finite(s)
  

  ## ask z3
  lhs = Implies(delta, texpr)
  rhs = Implies(texpr, delta)

  s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))

  print(f"solver: {s}.")
  print(f"result: {s.check()}.")

# private_test()



def ongoing_test():
  # s = Solver()
  # e = fpInfinity(FPSort(8,24),False)
  # t = FP('t',FPSort(8,24))
  # n = Int('n')
  # global_, x = Reals('global_ x')
  # texpr = fpLEQ(t,e)
  # delta = fpGEQ(fpAdd(RNE(),fpRealToFP(RNE(),x,Float32()),t),6.0)
  # lhs = Implies(delta,texpr)
  # rhs = Implies(texpr,delta)
  # s.add(global_==6.0, x==6.0, n==6)
  # s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))
  # result = s.check()
  
  # s = Solver()
  # e = Real('e')
  # s.add(e==1)
  # t = Real('t')
  # global_, x, z = Reals('global_ x z')
  # texpr = t <= e
  # n = Int('n')
  # s.add(n==1)
  # delta = z+t<=n
  # lhs = Implies(delta,texpr)
  # rhs = Implies(texpr,delta)
  # s.add(global_==1.0, x==1.0, z==1.0)
  # s.add(ForAll(t,Implies(0<=t,lhs)))
  # # s.add(ForAll(t,Implies(0<=t,rhs)))
  # # s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))
  # result = s.check()

  # s = Solver()
  # e = Real('e')
  # s.add(e==3)
  # t = Real('t')
  # global_, x = Reals('global_ x')
  # texpr = t <= e
  # n1, n2 = Ints('n1 n2')
  # s.add(n1==6, n2==9)
  # delta = And(x+t>=n1, x+t<n2)
  # lhs = Implies(delta,texpr)
  # rhs = Implies(texpr,delta)
  # s.add(global_==6.0, x==6.0)
  # # s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))
  # # s.add(ForAll(t,Implies(0<=t,lhs)))
  # s.add(ForAll(t,Implies(0<=t,rhs)))
  # result = s.check()
  
  # s = Solver()
  # e = Real('e')
  # s.add(e==3)
  # t = Real('t')
  # global_, x = Reals('global_ x')
  # texpr = t < e
  # n1, n2 = Ints('n1 n2')
  # s.add(n1==1, n2==4)
  # delta = And(x+t>=n1, x+t<n2)
  # lhs = Implies(delta,texpr)
  # rhs = Implies(texpr,delta)
  # s.add(global_==1.0, x==1.0)
  
  n1, n2 = Ints('n1 n2')
  global_, y, x = Reals('global_ y x')
  s = Solver()
  s.add(global_==1.0, y==1.0, x==1.0, n1==1, n2==2)
  s.add(And(x>n1, y<n2))
  
  # # s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))
  # s.add(ForAll(t,Implies(0<=t,lhs)))
  # s.add(ForAll(t,Implies(0<=t,rhs)))
  
  result = s.check()
  print(f"solver: {s}")
  print(f"result: {result}")

# ongoing_test()
