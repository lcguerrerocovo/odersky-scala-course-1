

def abs(x: Double) : Double = if(x > 0) x else -x
def goodEnough(x: Double, estimate: Double) : Double = {
  if(abs(x - estimate * estimate) < 0.0001) estimate
  else goodEnough(x, (estimate + x/estimate)/2)
}
def sqrt(x : Double) : Double = {
  goodEnough(x,1)
}

sqrt(4)
