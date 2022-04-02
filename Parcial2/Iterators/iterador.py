from typing import Any, Iterable, List


def ins(e : Any, ls : List[Any]) -> Iterable[Any]:
    yield [e, *ls]
    if ls:
        for i in ins(e, ls[1:]):
            yield [ls[0], *i]

def misterio(ls : List[Any]) -> Iterable[Any]:
    if ls:
        for m in misterio(ls[1:]):
            for i in ins(ls[0], m):
                yield i

    else:
        yield []

def suspenso1(ls : List[Any]) -> Iterable[Any]:
    for perm in misterio(ls):
        for elem in perm:
            yield elem

def suspenso(ls : List[Any]) -> Iterable[Any]:
    if ls:
        for m in misterio(ls[1:]):
            for i in ins(ls[0], m):
                for j in i: 
                    yield j

    else:
        yield []


if __name__ == "__main__":

    for m in misterio ([1,2,3]):
        print(m)

    sus1 : List[int] = []
    sus : List[int]  = []
    
    for m in suspenso([1,2,3]):
        sus.append(m)

    for m in suspenso1([1,2,3]):
        sus1.append(m)
    
    assert (sus == sus1)