  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (n)
  > RETURN n 
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  n
  +-----------------------------+
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Rob Reiner"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Oliver Stone"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Michael Douglas"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Martin Sheen"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Charlie Sheen"  }
  
  "labels": 
  {  "Movie"  },
  "properties": 
  {  "title": "Wall Street"  }
  
  "labels": 
  {  "Movie"  },
  "properties": 
  {  "title": "The American President"  }

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (movie:Movie)
  > RETURN movie.title
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  movie.title
  +-----------------------------+
  "Wall Street"
  "The American President"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (director {name: 'Oliver Stone'})--(movie)
  > RETURN movie.title
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  movie.title
  +-----------------------------+
  "Wall Street"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (:Person {name: 'Oliver Stone'})--(movie:Movie)
  > RETURN movie.title
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  movie.title
  +-----------------------------+
  "Wall Street"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (p)<-[r]-(movie)
  > WHERE p.title = "Wall Street"
  > RETURN movie.name
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  movie.name
  +-----------------------------+
  "Oliver Stone"
  "Michael Douglas"
  "Martin Sheen"
  "Charlie Sheen"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (movie)<-[r]-(:Person {name: 'Oliver Stone'})
  > RETURN movie.title
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  movie.title
  +-----------------------------+
  "Wall Street"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (:Person {name: 'Oliver Stone'})-[r]->(movie)
  > RETURN type(r)
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  type(r)
  +-----------------------------+
  "DIRECTED"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (actor)-[:ACTED_IN]->(wallstreet:Movie {title: 'Wall Street'})
  > RETURN actor.name
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  actor.name
  +-----------------------------+
  "Michael Douglas"
  "Martin Sheen"
  "Charlie Sheen"
  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (a)-[:ACTED_IN {role: 'Bud Fox'}]-(b)
  > RETURN a, b
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  a
  +-----------------------------+
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Charlie Sheen"  }
  
  "labels": 
  {  "Movie"  },
  "properties": 
  {  "title": "Wall Street"  }
  
  +-----------------------------+
  b
  +-----------------------------+
  
  "labels": 
  {  "Movie"  },
  "properties": 
  {  "title": "Wall Street"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Charlie Sheen"  }

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (actor)-[r:ACTED_IN]->(wallstreet {title: 'Wall Street'})
  > RETURN r.role
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  r.role
  +-----------------------------+
  "Gordon Gekko"
  "Carl Fox"
  "Bud Fox"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (charlie:Person {name: 'Charlie Sheen'}), (rob:Person {name: 'Rob Reiner'})
  > CREATE (rob)-[:`TYPE INCLUDING A SPACE`]->(charlie);
  > MATCH (n {name: 'Rob Reiner'})-[r:`TYPE INCLUDING A SPACE`]->()
  > RETURN type(r)
  
  Nodes created: 7 
  Edges created: 8 
  Edges created: 1 
  
  +-----------------------------+
  type(r)
  +-----------------------------+
  "TYPE INCLUDING A SPACE"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (michael {name: 'Michael Douglas'})-[p]->()
  > RETURN p
  
  Nodes created: 7 
  Edges created: 8 
  
  +-----------------------------+
  p
  +-----------------------------+
  
  "type": 
  [  "ACTED_IN"  ],
  "properties": 
  {  "role": "Gordon Gekko"  }
  
  "type": 
  [  "ACTED_IN"  ],
  "properties": 
  {  "role": "President Andrew Shepherd"  }

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH (wallstreet {title: 'Wall Street'})<-[r:ACTED_IN]-(actor)
  > DETACH DELETE actor;
  > MATCH (director)-[r]->(wallstreet {title: 'Wall Street'})
  > RETURN director.name AS DIRECTED
  
  Nodes created: 7 
  Edges created: 8 
  
  Nodes deleted: 3 
  Edges deleted: 6 
  
  +-----------------------------+
  DIRECTED
  +-----------------------------+
  "Oliver Stone"

  $ ./tests.exe <<-"EOF"
  > CREATE (charlie:Person {name: 'Charlie Sheen'}),
  >  (martin:Person {name: 'Martin Sheen'}),
  >  (michael:Person {name: 'Michael Douglas'}),
  >  (oliver:Person {name: 'Oliver Stone'}),
  >  (rob:Person {name: 'Rob Reiner'}),
  >  (wallStreet:Movie {title: 'Wall Street'}),
  >  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  >  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  >  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  >  (oliver)-[:DIRECTED]->(wallStreet),
  >  (thePresident:Movie {title: 'The American President'}),
  >  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  >  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  >  (rob)-[:DIRECTED]->(thePresident),
  >  (martin)-[:FATHER_OF]->(charlie);
  > MATCH
  > (charlie:Person {name: 'Charlie Sheen'}),
  > (martin:Person {name: 'Martin Sheen'})
  > CREATE (charlie)-[:ACTED_IN {role: 'Bud'}]->(:Movie {title: 'Free Money'}), (:Movie {title: 'Free Money'})<-[:ACTED_IN {role:'New Warden'}]-(martin),
  > (charlie)-[:ACTED_IN {role: 'Jake Peterson'}]->(:Movie {title: 'No Code of Conduct'}), (:Movie {title: 'No Code of Conduct'})<-[:ACTED_IN {role: 'Bill Peterson'}]-(martin);
  > MATCH (n {name: 'Charlie Sheen'})
  > RETURN n AS person
  
  Nodes created: 7 
  Edges created: 8 
  
  Nodes created: 2 
  Edges created: 4 
  
  +-----------------------------+
  person
  +-----------------------------+
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "name": "Charlie Sheen"  }

  $ ./tests.exe <<-"EOF"
  > CREATE
  >  (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  >  (timothy:Person {name: 'Timothy', age: 25}),
  >  (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  >  (andy)-[:KNOWS {since: 2012}]->(timothy),
  >  (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (a:Person)-[:KNOWS]->(b:Person)
  > WHERE a.name = "Andy"
  > RETURN b.name AS friends;
  > MATCH (a)<--(b)
  > WHERE a.name = "Peter"
  > DETACH DELETE a;
  > MATCH (a)-->(b)
  > WHERE a.name = "Andy"
  > RETURN b.name AS friend
  
  Nodes created: 3 
  Edges created: 2 
  
  +-----------------------------+
  friends
  +-----------------------------+
  "Timothy"
  "Peter"
  
  Nodes deleted: 1 
  Edges deleted: 1 
  
  +-----------------------------+
  friend
  +-----------------------------+
  "Timothy"


  $ ./tests.exe <<-"EOF"
  > CREATE
  >  (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  >  (timothy:Person {name: 'Timothy', age: 25}),
  >  (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  >  (andy)-[:KNOWS {since: 2012}]->(timothy),
  >  (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE (n.age < 30 AND n.name = 'Timothy') OR NOT (n.name = 'Timothy' OR n.name = 'Peter')
  > RETURN n.name AS name;
  > MATCH (n)
  > RETURN n AS Guys
  
  Nodes created: 3 
  Edges created: 2 
  
  +-----------------------------+
  name
  +-----------------------------+
  "Timothy"
  "Andy"
  
  +-----------------------------+
  Guys
  +-----------------------------+
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "age": 25,
  "name": "Timothy"  }
  
  "labels": 
  {  "Person"  },
  "properties": 
  {  "email": "peter_n@example.com",
  "age": 35,
  "name": "Peter"  }
  
  "labels": 
  {  "Swedish",
  "Person"  },
  "properties": 
  {  "belt": "white",
  "age": 36,
  "name": "Andy"  }

