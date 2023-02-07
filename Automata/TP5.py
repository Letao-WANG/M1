from time import time

transitions = {0: {"a": {0, 1}},
               1: {"a": {0}},
               2: {"b": {2}}}


class Auto:
    def __init__(self, init, final, trs):
        self.init = frozenset(init)
        self.final = frozenset(final)
        delta = {}
        for q in trs:
            delta[q] = {}
            for a in trs[q]:
                delta[q][a] = frozenset(trs[q][a])
        self.delta = delta
        self.cache = {}

    def next_state(self, q, a):
        """
        :param q: state
        :param a:
        :return: the set of destination states from state q reading letter a
        """
        assert q in self.delta
        if a in self.delta[q]:
            return self.delta[q][a]
        return frozenset()

    def next(self, s, a):
        """
        :param s: set of states
        :param a: letter
        :return: set of all states possible that can be reached from a state in S reading a
        """
        res = set()
        for q in s:
            res.update(self.next_state(q, a))
        return frozenset(res)

    def next_cache(self, s, a):
        key = (s, a)
        if key not in self.cache:
            r = self.next(s, a)
            self.cache[key] = r
            return r
        return self.cache[key]

    def epsilon_closure(self, q):
        """
        The set of states reachable by q through an epsilon transition
        :param q:
        :return:
        """
        res = self.next_state(q, '')
        while True:
            s = self.next(res, '')
            tmp_res = s.union(res)
            if len(res) == len(tmp_res):
                break
            res = tmp_res
        return res

    ## What is time complexity of next()?
    ## asssumptions: res.update(S) is linear


def run(auto, txt):
    """
    Check whether txt is accepted by auto without backtracking
    :param auto:
    :param txt:
    :return:
    """
    s = auto.init
    for a in txt:
        if len(s) == 0:
            return False
        # s = auto.next(s, a)
        s = auto.next_cache(s, a)
    return not s.isdisjoint(auto.final)


def in_union(auto1, auto2, txt):
    """Check that txt is in the union"""
    return run(auto1, txt) or run(auto2, txt)


def in_inter(auto1, auto2, txt):
    """Check that txt is in the intersection"""
    return run(auto1, txt) and run(auto2, txt)


def in_inter2(auto1, auto2, txt):
    s1 = auto1.init
    s2 = auto2.init
    for a in txt:
        if len(s1) == 0 or len(s2) == 0:
            return False
        s1 = auto1.next(s1, a)
        s2 = auto2.next(s2, a)
    return not s1.isdisjoint(auto1.final) and not s2.isdisjoint(auto2.final)


def naive_run_aux(auto, txt, q, i):
    if i >= len(txt):
        return q in auto.final
    a = txt[i]
    # for p in auto.next_state(q, a):
    # epsilon* a epsilon*
    for p in auto.epsilon_closure(auto.next_state(auto.epsilon_closure(q), a)):
        r = naive_run_aux(auto, txt, p, i + 1)
        if r:
            return True
    # Adding epsilon moves this is wrong because of infinite loop
    # for p in auto.next_state(q, ''):
    #     r = naive_run_aux(auto, txt, p, i)
    #     if r:
    #         return True
    return False


def naive_run_rec(auto, txt):
    for q in auto.init:
        r = naive_run_aux(auto, txt, q, 0)
        if r:
            return True
    return False


def naive_run(auto, txt):
    """
    Backtracking algorithm that returns:
            True,  if txt is recognized by the automaton
            False, otherwise.
        :param auto:
        :param txt: string
        :return:
        """
    stack = [(0, q) for q in auto.init]
    while len(stack) > 0:
        print(stack)
        (i, q) = stack.pop()
        if i == len(txt):
            if q in auto.final:
                return True
        else:
            a = txt[i]
            for p in auto.next_state(q, a):
                stack.append((i + 1, p))
    return False


bad = Auto([0], [2], {
    0: {'a': {0, 1}},
    1: {'a': {0},
        'b': {2}}})

# for the bad automaton, the number of configuration explored is exponential
# We call N(q0, k) the number of configurations explored in q0 with k letter remaining for a text 'aaaaa...'
# N(q0, k) = N(q0, k-1) + N(q1, k-1)
# N(q0, k) = N(q0, k-1) + N(q0, k-2)

for _ in range(100):
    if run(bad, 'aaaab') == False:
        print("false")

# for i in range(100):
#     txt = 'a' * i
#     t0 = time()
#     # naive_run(bad, txt)
#     run(bad, txt)
#     t1 = time()
#     print(i, t1 - t0)
