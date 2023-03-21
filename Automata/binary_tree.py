from typing import List, Dict


class BinaryTree:
    """
    Binary tree structure
    """

    def __init__(self, label: str, left: 'BinaryTree' = None, right: 'BinaryTree' = None):
        self.label = label
        self.left = left
        self.right = right


class ATA:
    """
    Alternating tree automata structure
    """

    def __init__(self, states: List[str], init_states: List[str], delta: Dict[str, str]):
        self.states = states
        self.init_states = init_states
        self.delta = delta


def choose_symbol(s):
    """
    choose the symbol of the opening and closing parentheses
    """
    opening_paren = s.index('(')
    closing_paren = s.index(')')
    symbol = s[opening_paren + 1:closing_paren]
    return symbol


def is_recognized_rec(tree: BinaryTree, ata: ATA, current_states: List[str]) -> bool:
    """
    If the tree (in BinaryTree class) could be recognized by an alternating tree automata structure.
    :param tree: binary tree
    :param ata: alternating tree automata
    :param current_states: the nodes that this algorithm is traversing
    :return: if the tree could be recognized by an alternating tree automata
    """
    next_states_left = []
    next_states_right = []

    if tree is None:
        return True

    for state in current_states:
        formula = ata.delta.get(state)
        if formula is not None:
            parts = formula.split()
            i = 0
            while i < len(parts):
                part = parts[i]
                if 'lab' in part and tree is not None:
                    symbol = choose_symbol(part)
                    if symbol != tree.label:
                        print("No match label!, expected " + str(symbol) + ", but got " + str(tree.label))
                        return False
                    i += 1
                elif part == '↓1':
                    next_states_left.append(parts[i + 1])
                    i += 2
                elif part == '↓2':
                    next_states_right.append(parts[i + 1])
                    i += 2
                elif part == '•':
                    next_states_right.append(parts[i])
                    i += 2
                else:
                    i += 1

    left_result = is_recognized_rec(tree.left, ata, next_states_left)
    right_result = is_recognized_rec(tree.right, ata, next_states_right)
    return left_result and right_result


def is_recognized(tree: BinaryTree, ata: ATA) -> bool:
    return is_recognized_rec(tree, ata, ata.init_states)


# Define the ATA states, initial states, and transition function (delta)
states = ['q0', 'q1', 'q2']
init_states = ['q0']
delta = {
    'q0': 'lab(a) ↓1 q1 ↓2 q2',
    'q1': 'lab(b) ↓1 q2',
    'q2': 'lab(c)'
}

# ----------- Example------------ #
ata = ATA(states, init_states, delta)

#      a (q0)
#    /        \
#   b (q1)  c (q2)
#  /
# c (q2)
tree = BinaryTree('a', BinaryTree('b', BinaryTree('c')), BinaryTree('c'))

# Check if the tree is recognized by the ATA's language
result = is_recognized(tree, ata)
print(f"First tree belongs to language of an automaton: {result}")
print("--------------------------------------------------------")

# ----------- Counterexample------------ #

#     a (q0)
#    /      \
#   c        a

tree = BinaryTree('a', BinaryTree('c'), BinaryTree('a'))

# Check if the tree is recognized by the ATA's language
result = is_recognized(tree, ata)
print(f"Second tree belongs to language of an automaton: {result}")

