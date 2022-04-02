from typing import List, Callable


X = 1
Y = 1
Z = 3

a = ((X+Y) % 5) + 3 # a = 5
b = ((Y+Z) % 5) + 3 # b = 7


###############################
# 4.a)                        #
###############################

def F(n : int, a : int = a, b : int = b) -> int:
    if (0 <= n < a * b):
        return n
    
    return sum([F(n-b*i) for i in range(1,a+1)])


###############################
# 4.b)                        #
###############################




def F_tail(n : int, a : int = a, b : int = b) -> int:
    
    ab : int = a*b
    # CLOSURE GOES BRRRRRRRRRRRRRRRRR
    # basically Linear time O(a*n), O(a*b) space baby.
    def _F_tail(current : int, mem : List[int]) -> int:
        if (current > n):
            return mem[n % ab]
        
        for i in range(ab):
            mem[i]  = sum([mem[(i+b*j) % ab] for j in range(1,a+1)])

        return _F_tail(current+ab,mem)

    return _F_tail(a*b, [i for i in range(a*b)])


###############################
# 4.c)                        #
###############################

# translation is almost direct
def F_loop(n : int, a : int = a, b : int = b) -> int:
    mem : List[int] = [i for i in range(a*b)] # `mem`` is the `mem` param of _F_tail
    ab  : int = a*b
    # In each recursive step
    # we add `ab`... 
    # which is done automatically 
    # be the range
    #     v
    #     v  We make explicit how many iterations
    #     v       we are going to make
    #     v               v
    #     v               v
    for  _ in range(a*b,n+1,ab):  #(36,37,35)
        for i in range(ab): # same step
            mem[i]  = sum([mem[(i+b*j) % ab] for j in range(1,a+1)]) 

    return mem[n % ab] 

def main(log : bool = False) -> None : 

    def getLogFun() -> Callable[[int,int,int,int],None]:
        if (log):
            def logger(i : int, f : int, tr: int, gl : int) -> None :
                print(f"For {i}: ")
                print(f"F({i}): {f}")
                print(f"F_tail({i}): {tr}")
                print(f"F_loop({i}): {gl}")
                print(f"is tail rec equal? {f==tr}")
                print(f"is F_loop equal? {f==gl}")
                print("\n\n")
            return logger
        else:
            return (lambda _1,_2,_3,_4: None)

    logger :  Callable[[int,int,int,int],None] = getLogFun()

    for i in range(100):
        f  : int = F(i)
        tr : int = F_tail(i)
        gl : int = F_loop(i)
        logger(i,f,tr,gl)
        assert(f==gl)
        assert(f==tr)
        
        
        

if __name__ == "__main__":
    main(log = False)
    
