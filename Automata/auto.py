from time import time


class Auto:
    def __init__(self, init, final, trs):
        self.init = frozenset(init)
        self.final = frozenset(final)
        delta = {}
        for q, a, p in trs:
            if q not in delta:
                delta[q] = {}
            if a not in delta[q]:
                delta[q][a] = set()
            delta[q][a].add(p)
        for q in delta:
            for a in delta[q]:
                delta[q][a] = frozenset(delta[q][a])
        self.delta = delta
        self.cache = {}

    def is_final(self, q):
        return q in self.final

    def next_state(self, q, a):
        if q not in self.delta:
            return frozenset([])
        if a not in self.delta[q]:
            return frozenset([])
        return self.delta[q][a]

    def next(self, s, a):
        res = []
        for q in s:
            res.append(self.next_state(q, a))
        return frozenset(res)

    def next_cache(self, s, a):
        key = (s, a)
        if key not in self.cache:
            r = self.next(s, a)
            self.cache[key] = r
            return r
        return self.cache[key]


def naive_run_aux(auto, txt, q, i):
    if i >= len(txt):
        return auto.is_final(q)
    a = txt[i]
    for p in auto.next_state(q, a):
        r = naive_run_aux(auto, txt, p, i + 1)
        if r:
            return r
    return False


def naive_run(auto, txt):
    for q in auto.init:
        r = naive_run_aux(auto, txt, q, 0)
        if r:
            return r
    return False


def run(auto, txt):
    s = auto.init
    i = 0
    while i < len(txt) and len(s) > 0:
        a = txt[i]
        s = auto.next_cache(s, a)
        i = i + 1
    return not (s.isdisjoint(auto.final))


auto0 = Auto([0], [2], [(0, 'a', 0), (0, 'a', 1), (1, 'a', 0), (1, 'b', 2), ])

for i in range(9999, 10000):
    txt = i * 'a'
    t0 = time()
    run(auto0, txt)
    print(i, time() - t0)
