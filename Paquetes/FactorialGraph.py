import networkx as nx
import matplotlib.pyplot as plt

def draw_factorial_tree(n):
    G = nx.DiGraph()
    
    def build_tree(k, parent=None):
        if k == 0:
            return
        if parent is not None:
            G.add_edge(parent, k)
        build_tree(k - 1, k)

    build_tree(n, 'Factorial({})'.format(n))

    plt.figure(figsize=(8, 6))
    pos = nx.spring_layout(G, seed=42)
    nx.draw(G, pos, with_labels=True, node_color='lightblue', edge_color='gray', node_size=2000, font_size=10)
    plt.title('Árbol de llamadas - Recursión de Pila (Factorial)')
    plt.show()

def draw_tail_factorial_tree(n):
    G = nx.DiGraph()

    def build_tree(k, acc, parent=None):
        if k == 0:
            return
        node_label = '({}, {})'.format(k, acc)
        if parent is not None:
            G.add_edge(parent, node_label)
        build_tree(k - 1, k * acc, node_label)

    build_tree(n, 1, 'FactorialTail({})'.format(n))

    plt.figure(figsize=(8, 6))
    pos = nx.spring_layout(G, seed=42)
    nx.draw(G, pos, with_labels=True, node_color='lightgreen', edge_color='gray', node_size=2000, font_size=10)
    plt.title('Árbol de llamadas - Recursión de Cola (Factorial)')
    plt.show()
