from math import gcd, log, ceil, floor
from random import choice
from AKS import aks


def period(N: int, k: int) -> [(int, int)]:
    f_x, T, q = 0, 0, 1
    t = []
    while f_x != 1:
        f_x = (k * q) % N
        q = f_x
        T += 1
        t.append((T, q))
    return t


def shor(N: int):
    if N % 2 == 0:
        return f"{N} is even"
    if aks(N) == "Prime":
        return f"{N} is a prime"

    power = set()
    for _ in range(2, 11):
        power.add(ceil(log(N, _)) == floor(log(N, _)))
    if True in power:
        return f"{N} is a n**x"

    else:
        return shor_run(N)


def shor_run(N: int) -> [int]:
    k = choice(range(1, N))
    if gcd(N, k) != 1:
        factors = [gcd(N, k), int(N / gcd(N, k))]
        return factors
    else:
        Tq = period(N, k)
        if len(Tq) % 2 == 0:
            p = Tq[int(len(Tq) / 2) - 1][1]
            if p + 1 != N:
                factors = [gcd(p + 1, N), gcd(p - 1, N)]
                return factors
            else:
                shor_run(N)
        else:
            shor_run(N)
