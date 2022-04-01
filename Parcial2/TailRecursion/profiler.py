from typing import List, Tuple, Dict 
from TailRec import F,F_tail, F_loop
from timeit import timeit 
import csv



def wrapper(func, n, a, b):
    def wrapped():
        return func(n,a,b)
    return wrapped

class FunctionTimes():
    def __init__(self,n : int,a : int,b : int) -> None:
        self._a   = a
        self._b   = b 
        self._n  = n 
        self._F_time      = timeit(wrapper(F,self._n,self._a,self._b),      number=100)
        self._F_tail_time = timeit(wrapper(F_tail,self._n,self._a,self._b), number=100)
        self._F_loop_time = timeit(wrapper(F_loop,self._n,self._a,self._b), number=100)
    
    def getTimes(self) -> Tuple[float,float,float]:
        return (self._F_time,self._F_tail_time,self._F_loop_time)
    
    def getInputSize(self) -> int:
        return self._n 

    def getTimesDict(self) -> Dict[str,float]:
        return {"F":self._F_time, "F_tail":self._F_tail_time, "F_loop":self._F_loop_time}


def main():
    times : List[FunctionTimes] = [FunctionTimes(n,2,3) for n in range(10,200)]
    with open('times.csv', 'w', newline='') as csvfile:
        fieldnames = ['size', 'time F', 'time F_tail', 'time F_loop']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for timeObj in times:
            (f_time,f_tail_time,f_loop_time) = timeObj.getTimes()
            n = timeObj.getInputSize()
            writer.writerow({'size': n, 'time F': f_time,'time F_tail': f_tail_time, 'time F_loop': f_loop_time})

if __name__ == "__main__":
    main ()


