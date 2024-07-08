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




def ongoing_test():
  
  # # s.add(ForAll(t,Implies(0<=t,And(lhs,rhs))))
  # s.add(ForAll(t,Implies(0<=t,lhs)))
  # s.add(ForAll(t,Implies(0<=t,rhs)))
  
  result = s.check()
  print(f"solver: {s}")
  print(f"result: {result}")
