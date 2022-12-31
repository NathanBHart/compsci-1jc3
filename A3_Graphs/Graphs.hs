module Graphs where

-- Graphs
-- Name: Nathan Hart
-- Date: November 15th 2022


{- Datatype Declarations: -------------------------------------------

    Synonym Types: ---
    NodeID: Unique integer representing a Node
    Vertices: A list of NodeIDs representing the directed connections
        from one node to another.
    GraphElement: A tuple containing a unique node, as well as the
        vertices it has.

    Newtype types: ---
    Graph: A polymorphic list of GraphElements of a homogenous type

    Datatypes: ---
    Node: A node contains a NodeID and a polymorphic value.        -}
    
type NodeID = Int
data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)

type Vertices = [NodeID]
type GraphElement a = (Node a, Vertices)
newtype Graph a = Graph [GraphElement a]
  deriving (Show,Eq)


{- Example Graph Construction: -------------------------------------

         +---+
    A -> C <-+
    |    |
    v    |
    B <--+                                                        -}

nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = Graph [(nodeA,[1,2])
                ,(nodeB,[])
                ,(nodeC,[1,2])]


{- Function: maxNodeID ---------------------------------------------
    
    Description: Finds the maximum nodeID present in a graph of nodes
        and returns that value wrapped in the Maybe data structure.

    Input: graph :: Graph a, The graph to search

    Output: Maybe NodeID
        | Nothing -> Graph is empty
        | Just NodeID -> Graph has one or more nodes               -}

maxNodeID :: Graph a -> Maybe NodeID
maxNodeID graph = let

    identity = getNodeID.fst

    mID :: Graph a -> Maybe NodeID
    mID (Graph []) = Nothing
    mID (Graph [x]) = Just (identity x)
    mID (Graph (x:y:rest))
      | identity x > identity y = mID (Graph (x:rest))
      | otherwise               = mID (Graph (y:rest))
    
    in mID graph


{- Function: insertNode ---------------------------------------------
    
    Description: Inserts a new node into a graph, assinging it a
        unique NodeID equivalent to the previously largest NodeID in
        the graph + 1. This node is initialized with a value (v).

    Input: v :: a, Any value of the same type as the graph's
        graph :: Graph a, the unmodified graph

    Output: Graph a, the modified graph                            -}

insertNode :: a -> Graph a -> Graph a
insertNode v graph@(Graph elements) = Graph ((newNode, []):elements)
    where newNode = Node newNodeID v
          newNodeID = toNewID(maxNodeID graph)
          toNewID Nothing = 0
          toNewID (Just x) = x + 1


{- Function: removeNode ---------------------------------------------

    Description: Removes a node of a given NodeID from a graph and
        all edges pointing to it and returns the modified graph.

    Input: nID :: NodeID, an integer representing the node to remove
        (Graph nodes) :: Graph a, the unmodified graph

    Output: Graph a, the modified graph                            -}

removeNode :: NodeID -> Graph a -> Graph a 
removeNode nID (Graph nodes) = Graph newList
    where newList = [(x, cull edges) | (x, edges) <- nodes, nID /= getNodeID x]
          cull list = [x | x <- list, nID /= x]


{- Function: lookupNode ---------------------------------------------

    Description: Searches through a graph for a node with the
        supplied NodeID. Returns that node.

    Input: nID :: NodeID, an integer representing the node to find
        Graph [GraphElement a] :: Graph a, graph to search

    Output: Maybe (Node a)
        | Nothing -> Graph does not contain node with NodeID
        | Just (Node a) -> Graph contains node with NodeID         -}

lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph []) = Nothing
lookupNode nID (Graph ((x, _):xs))
    | getNodeID x == nID = Just x
    | otherwise          = lookupNode nID (Graph xs)


{- Function: insertEdge ---------------------------------------------

    Description: Given a tuple containing two NodeIDs and a graph, it
        will form an edge in the supplied graph connecting a node
        with the first NodeID to the node with the second NodeID only
        if both nodes are in the graph. Will return the augmented
        graph if both nodes are found, or nothing if either of the
        nodes are not found.

    Input: (n1, n2) :: (NodeID, NodeID), couple of integers
                          representing the nodes to connect
        g :: Graph a, unmodified graph

    Output: Maybe (Graph a)
        | Nothing -> Graph does not contain either node specified
        | Just (Graph a) -> Graph contains both nodes              -}

insertEdge :: Eq a => (NodeID, NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _ (Graph []) = Nothing
insertEdge (n1,n2) g@(Graph graph)
  | containsBothNodes = Just $ Graph updatedGraphItems
  | otherwise         = Nothing
  where

    -- Determines if both nodes are in the graph
    containsBothNodes :: Bool
    containsBothNodes = isSomething n1 && isSomething n2
        where isSomething x = lookupNode x g /= Nothing

    {- List comp to generate a new [(Node a, [NodeID])] with the
       added connection -}
    updatedGraphItems = [
        value | item@(node, edges) <- graph,
        let value = if isTargetNode then modifiedItem else item
            isTargetNode = getNodeID node == n1
            modifiedItem = (node, n2 `addTo` edges)
            x `addTo` list
                | x `elem` list = list
                | otherwise = x:list
        ]
    

-- Graphs for testing, see test plan --

{- From above, also used in testing
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = Graph [(nodeA,[1,2])
                ,(nodeB,[])
                ,(nodeC,[1,2])]
-}

edgelessGraph :: Graph Char
edgelessGraph = Graph [(nodeA,[])
                ,(nodeB,[])
                ,(nodeC,[])]

emptyGraph :: Graph a
emptyGraph = Graph []

negNode :: Node Char
negNode = Node (-2) 'A'

negNodeGraph :: Graph Char
negNodeGraph = Graph [(negNode, [])]