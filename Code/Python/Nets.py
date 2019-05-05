#!/usr/bin/env python

""" Practical 6.4.1 Extra-credit - A Python script homologous to R script 'Nets.R', for visualising the QMEE CDT collaboration network """

__author__ = "Saul Moore sm5911@imperial.ac.uk"
__version__ = "0.0.1"

import csv
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Read in 'edge' data ('connectivity')
file1 = open('../Data/QMEE_Net_Mat_edges.csv', 'r')
edges = pd.read_csv(file1)
file1.close()

# Read in 'node' data ('species')
file2 = open('../Data/QMEE_Net_Mat_nodes.csv', 'r')
nodes = pd.read_csv(file2)
file2.close()

# Record coordinates of non-zero values from the upper triangle of pandas adjacency matrix
source_node, target_node = np.where(np.triu(edges > 0))

# Zip values as list of tuple coordinates and record x and y coords in separate lists
node_list = list(zip(source_node.tolist(), target_node.tolist()))
# source_node = [x[0] for x in edge_list]
# target_node = [y[1] for y in edge_list]

# Record corresponding node names as list of strings
source_node_names = list(nodes['id'][source_node])
target_node_names = list(nodes['id'][target_node])
node_name_list = zip(source_node_names, target_node_names)

# Use coords to retrieve weights (connectivity)
weights = [edges.iloc[i,j] for i,j in node_list]

# Generate pandas dataframe in correct layout for network
network_df = pd.DataFrame(zip(source_node_names, target_node_names, weights), columns=['source', 'target', 'weight'])

# Make networkx diagram
G = nx.Graph()
G.add_nodes_from(node_list)
G.add_edges_from(weights)
nx.draw(G, node_size=500)# , labels=node_list, with_labels=True)
plt.show()

G = nx.from_pandas_dataframe(df=network_df,source='source',target='target',edge_attr='weight')

pos = nx.fruchterman_reingold_layout(G)
G = nx.draw_networkx_nodes(G, pos, node_color='r', node_shape='o', alpha=1.0)

G = nx.draw_networkx_edges(G, pos)

nx.draw_networkx_labels()
# nx.draw_circular(G)

plt.show(G)
plt.savefig('../Results/Network.svg')

# weights = edges.unstack().reset_index() # .drop('level_0', axis=1)
# source_node_names = list(nodes['id'][source_node])
# target_node_names = list(nodes['id'][target_node])

# source_node_list = []
# for i in source_node:
#     source_node_list.append(nodes['id'][i])

# target_node_list = []
# for i in target_node:
#     target_node_list.append(nodes['id'][i])
# edge_array = np.array(edges)
# G = nx.Graph(edge_array)
# G = nx.convert.to_networkx_graph(edge_array)
# G = nx.from_numpy_matrix(edge_array)
# G.add_edges_from(edge_list)
# G.add_nodes_from(node_list)


# node_names, adj_idx = np.unique(edge_array, return_inverse=True)
# adj_idx = adj_idx.reshape(-1, 2)
# tuple(map(tuple, edge_array))
# tuple([tuple(row) for row in edge_array])

# G = nx.from_pandas_dataframe(df=edges,source=list(nodes['id']),target=list(nodes['id']),edge_attr=list(nodes['Pis']))

# G = nx.Graph()
# G.add_edge()
# G=nx.Graph()
# G.add_edge(1,2)
# G.add_node(42)
# print(sorted(G.nodes()))
# print(sorted(G.edges()))
# G=nx.from_numpy_matrix
# G.edges(G, data=TRUE)
