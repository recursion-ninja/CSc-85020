import random
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.colors import LinearSegmentedColormap, Normalize
import igraph as ig


def write_graph( fig, name ):
    output_directory = 'assets'
    output_format    = 'png'
    output_filepath  = "".join([ output_directory, '/', 'Network-', name, '.', output_format ])
    fig.savefig( output_filepath, transparent=True )


def generate_example():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i % 3 == 0:
    	v.update_attributes({'NodeType': 'steelblue'})
    elif i % 5 == 0:
    	v.update_attributes({'NodeType': 'salmon'})    
    else:
        v.update_attributes({'NodeType': 'white'})

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color= g.vs['NodeType'],
  )
  write_graph( fig, 'Stable' )

def generate_no_sinks():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i % 3 == 0:
    	v.update_attributes({'NodeType': 'steelblue'})
    else:
        v.update_attributes({'NodeType': 'white'})

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color= g.vs['NodeType'],
  )
  write_graph( fig, 'No-Sinks' )
 
def generate_failed_nodes():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i % 3 == 0:
    	v.update_attributes({'NodeType': 'steelblue'})
    elif i in [7,8,10]:
    	v.update_attributes({'NodeType': 'black'}) 
    else:
        v.update_attributes({'NodeType': 'white'})

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color= g.vs['NodeType'],
  )
  write_graph( fig, 'Failed-Nodes' )


def generate_science_network():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i in [0,1,5,6,7,9]:
    	v.update_attributes({'NodeType': 'steelblue'})
    elif i in [2,4,10]:
    	v.update_attributes({'NodeType': 'black'}) 
    else:
        v.update_attributes({'NodeType': 'white'})

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color= g.vs['NodeType'],
  )
  write_graph( fig, 'Science' )

def generate_battlefield_network():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i in [0,1,5,2,10,11]:
    	v.update_attributes({'NodeType': 'black'}) 
    else:
        v.update_attributes({'NodeType': 'steelblue'})

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color= g.vs['NodeType'],
  )
  write_graph( fig, 'Battlefield' )

def generate_random_walk():
  g = ig.Graph.Famous('Frucht')
  
  for i, v in enumerate(g.vs):
    if i % 3 == 0:
    	v.update_attributes({'NodeType': 'steelblue'})
    elif i in [7,8,10]:
    	v.update_attributes({'NodeType': 'black'}) 
    else:
        v.update_attributes({'NodeType': 'white'})
        
  for i, e in enumerate(g.es):
    if i in [1,5,11]:
    	e.update_attributes({'EdgeColor': 'red'})
    else:
    	e.update_attributes({'EdgeColor': 'black'}) 

  fig, ax = plt.subplots(figsize=(5,5))
  ig.plot(
    g,
    target=ax,
    vertex_size=0.1,
    vertex_color=g.vs['NodeType'],
    edge_color=g.es['EdgeColor']
  )
  write_graph( fig, 'Random-Walk' )


def main():
    generate_example()
    generate_no_sinks()
    generate_failed_nodes()
    generate_science_network()
    generate_battlefield_network()
    generate_random_walk()


if __name__ == "__main__":
    main()
