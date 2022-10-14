import sys
import itertools as it
import matplotlib.pyplot as plt
import networkx as nx
from steinlib.instance import SteinlibInstance
from steinlib.parser import SteinlibParser

stein_file = "data/test.std"


# stein_file = "data/B/b02.std"


# procedure:
# complete_graph(nodes)
# minimimal_spanning_tree(graph)
# all_pairs_dijkstra(graph)

# draw a graph in a window
def print_graph(graph, terms=None, sol=None):
    pos = nx.kamada_kawai_layout(graph)

    nx.draw(graph, pos, with_labels=True)
    if (not (terms is None)):
        nx.draw_networkx_nodes(graph, pos, nodelist=terms, node_color='r')
    if (not (sol is None)):
        nx.draw_networkx_edges(graph, pos, edgelist=sol, edge_color='r')
    plt.show()
    return


# verify if a solution is correct and evaluate it
def eval_sol(graph, terms, sol):
    graph_sol = nx.Graph()
    for (i, j) in sol:
        graph_sol.add_edge(i, j, weight=graph[i][j]['weight'])

    # is sol a tree
    if (not (nx.is_tree(graph_sol))):
        print("Error: the proposed solution is not a tree")
        return -1

    # are the terminals covered
    for i in terms:
        if not i in graph_sol:
            print("Error: a terminal is missing from the solution")
            return -1

    # cost of solution
    cost = graph_sol.size(weight='weight')

    return cost


def approx_steiner(graph, terms):
    """
    compute a approximate solution to the steiner problem
    """
    # Find the shortest weighted paths of the original graph
    len_path = dict(nx.all_pairs_dijkstra(graph))
    # The complete graph of terminals
    comp_graph = nx.complete_graph(terms)
    # Add weight to the edges
    # print(comp_graph.edges)
    for e in comp_graph.edges:
        comp_graph[e[0]][e[1]]['weight'] = len_path[e[0]][0][e[1]]
        # print(e[0], "/", e[1], ":", comp_graph[e[0]][e[1]]['weight'])
    # The minimum spanning tree of the complete graph
    min_span_tree = nx.minimum_spanning_tree(comp_graph)
    res = []
    # path to edges
    for e in min_span_tree.edges:
        for i in range(len(len_path[e[0]][1][e[1]]) - 1):
            res.append((len_path[e[0]][1][e[1]][i],
                        len_path[e[0]][1][e[1]][i + 1]))
    # return a list of edges
    # print(res)
    return res


# class used to read a steinlib instance
class MySteinlibInstance(SteinlibInstance):
    my_graph = nx.Graph()
    terms = []

    def terminals__t(self, line, converted_token):
        self.terms.append(converted_token[0])

    def graph__e(self, line, converted_token):
        e_start = converted_token[0]
        e_end = converted_token[1]
        weight = converted_token[2]
        # print ("weight: " + str(weight))
        self.my_graph.add_edge(e_start, e_end, weight=weight)


if __name__ == "__main__":
    my_class = MySteinlibInstance()
    with open(stein_file) as my_file:
        my_parser = SteinlibParser(my_file, my_class)
        my_parser.parse()
        terms = my_class.terms
        graph = my_class.my_graph

        # graph_c = nx.complete_graph(4)
        # T = nx.minimum_spanning_tree(graph_c)
        # print("Tree: " + str(T))
        # len_path = dict(nx.all_pairs_dijkstra(T))
        # print("Dict: " + str(len_path))
        # print("graph: " + str(graph_c))
        # graph[1][2]["weight"] = 2
        # print_graph(graph, terms)

        sol = approx_steiner(graph, terms)
        print_graph(graph, terms, sol)
        print(eval_sol(graph, terms, sol))

# comparer two parameters with interval, confidence
