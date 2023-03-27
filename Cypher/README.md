### An implementaion of Cypher mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Drumov Maxim, Drumov.M.A@gmail.com

Features done:

- Cypher AST
- Cypher Parser  
- Cypher Interpreter 
- Implemented commands:
  - CREATE used to create nodes and edges;
  - MATCH Used to set the search parameters for the graph. In MATCH and CREATE, you can create and query edges both ways: ()-->() and ()<--(), multiedges ()-->()<--() queries are not supported;
  - WHERE serves as a label and property check. WHERE is always used with MATCH;
  - DETACH DELETE used to remove vertices and edges. When you delete a node, all edges connected to it are deleted;
  - DELETE is used to remove vertices and edges. You can only delete nodes that have no ties.

Features in progress (and TODOs):

- SET

