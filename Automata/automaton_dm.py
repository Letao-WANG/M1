from typing import List


class BinaryTree:
    def __init__(self, label: str, left: 'BinaryTree' = None, right: 'BinaryTree' = None):
        self.label = label
        self.left = left
        self.right = right

    def preorder(self) -> List[str]:
        result = [self.label]
        if self.left:
            result.extend(self.left.preorder())
        if self.right:
            result.extend(self.right.preorder())
        return result


class Automaton:
    def __init__(self, states: List[str], initial_state: str, transition_function):
        self.states = states
        self.initial_state = initial_state
        self.transition_function = transition_function

    def accepts(self, input_str: str) -> bool:
        current_state = self.initial_state
        for char in input_str:
            current_state = self.transition_function.get(current_state, {}).get(char)
            if current_state is None:
                return False
        return current_state in self.states


# Example usage:
tree = BinaryTree('a',
                  BinaryTree('b',
                             BinaryTree('c'),
                             None),
                  BinaryTree('c'))

preorder_str = ''.join(tree.preorder())
print(f"Pre-order traversal: {preorder_str}")  # Output: abcac

# Define the automaton states, initial state, and transition function (delta)
states = ['q0', 'q1', 'q2']
initial_state = 'q0'
transition_function = {
    'q0': {'a': 'q1'},
    'q1': {'b': 'q2'},
    'q2': {'c': 'q2'}
}

automaton = Automaton(states, initial_state, transition_function)
result = automaton.accepts(preorder_str)
print(f"Tree belongs to the automaton's language: {result}")  # Output: True
