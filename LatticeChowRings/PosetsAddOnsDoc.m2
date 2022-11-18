document {
	Key => {coatoms, (coatoms, Poset), (coatoms, Poset, Thing)},
	
	Headline => "of a poset or an element of a poset",
	
	Usage => "coatoms P, \n coatoms(P, x)",
	
	Inputs => {
	    	"P" => Poset,
	    	"x" => Thing => {"an element of the poset"},
		},
	
	Outputs => {
		List => {"of coatoms of the poset or element of the poset"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"The coatoms of a poset P with maximal element 1 are precisely the elements covered
	    by 1.  Equivalently, they are the atoms of the dual poset of P."},
	
	PARA {"The coatoms of an element x in a poset P having a minimal element 0 are the coatoms
	    of the interval [0, x]."},
	
	EXAMPLE {
		"B = booleanLattice 4;",
		"coatoms B",
		"coatoms(B, \"1011\")"
		},
	    
	    }


document {
	Key => {elementaryDivisors, (elementaryDivisors, Poset, Thing)},
	
	Headline => "of a poset element",
	
	Usage => "elementaryDivisors(P, x)",
	
	Inputs => {
	    	"P" => Poset => {"a finite poset with a unique minimal element"},
		"x" => Thing => {"an element of the poset"},
		},
	
	Outputs => {
		List => {"of elementary divisors of the poset element"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	    
	PARA {"Given a finite poset P with a unique minimal element 0 and a non-minimal
	    element x of P, the elementary divisors x_1,..., x_n of x are the maximal elements among
	    the set of irreducible elements of P that are less than or equal to x.   They have the 
	    property that there is a poset isomorphism from the product of the intervals [0, x_j] 
	    for j = 1,..., n to the interval [0, x] that sends the element (0,...,0, x_j,0,...,0) 
	    of the product to the element x_j for each j."},
	    
	PARA {"For the lattice of divisors of a positive integer N, the elementary divisors of N
	    are simply the largest prime powers dividing N, and the poset isomorphism realizing
	    the lattice as a product of irreducible intervals is just given by multiplication."},
	    
	EXAMPLE {
		"D = divisorPoset 300;",
		"elementaryDivisors(D, 300)",
		"elementaryDivisorIsomorphism(D, 300)",
		},
	        
	Caveat => {"This function does not check whether L is a lattice or G is a building set
	    in L since it is usually called by methods such as ", TO isBuildingSet, " and ", 
	    TO nestedSetComplex, " which can perform such checks."
	    }, 
		
	SeeAlso => {elementaryDivisorIsomorphism, factors, irreducibles, isIrreducible}
	    }		
						


document {
	Key => {irreducibleFactors, (irreducibleFactors, Poset)},
	
	Headline => "decompose a lattice as product of irreducible factors",
	
	Usage => "irreducibleFactors L",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
		},
	
	Outputs => {
		List => {"of irreducible lattices whose product is isomorphic to the given lattice"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"A lattice is irreducible if it is not isomorphic to a product of two posets, each
	    having at least two elements.  Every finite lattice L can be factored into a product
	    irreducible lattices.  This function computes those factors based on an algorithm of
	    Markowsky, which reconstructs the irreducible factors of L from the connected 
	    components of a certain bipartite graph associated to L.  See ", 
	    TO irreduciblesGraph, " for more information."},
	
	EXAMPLE {
		"B = booleanLattice 4;",
		"hasse = P -> (H = hasseDiagram P; digraph(apply(vertexSet H, i -> (vertexSet P)#i), adjacencyMatrix H) )",
		"hasse B",
		"(irreducibleFactors B)/hasse",
		},
	    
	PARA {"The isomorphism between the product of the irreducible factors and L is given
	    as follows:  Each element M of the product poset has entries that are sets 
	    of meet-irreducible elements of L.  The isomorphism assigns to M the unique element 
	    x of L such that the set of meet-irreducible elements not greater than or equal to x 
	    is precisely the union of the sets in M."},	 
	    
	EXAMPLE {
		"L = poset({0, 1, a, b, c, d}, {{0, a}, {0, b}, {a, c}, {b, c}, {b, d}, {c, 1}, {d, 1}})",
		"hasse L",
		"F = irreducibleFactors L;",
		"F/hasse",
		"isomorphism(productPoset F, L)",
		"select(meetIrreducibles L - set{first minimalElements L} , y -> not compare(L, c, y) )"
		},
	    	    
	SeeAlso => {irreducibles, irreduciblesGraph, isIrreducible}

	    }

document {
	Key => {irreducibles, (irreducibles, Poset), (irreducibles, Matroid)},
	
	Headline => "all irreducible elements of a lattice",
	
	Usage => "irreducibles L, \n irreducibles M",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
		"M" => Matroid => {"a simple matroid"},
		},
	
	Outputs => {
		List => {"of all irreducible elements of the poset"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Given a lattice L with minimal element 0, a non-minimal element x of L is
	    irreducible if the interval [0, x] is not isomorphic to a product of two posets, each
	    having at least two elements.  This function finds all the irreducible elements of L."},
	
	PARA {"Every Boolean lattice on at least two elements is reducible since it is isomorphic 
	    to a product of chains of length two.  So, the irreducibles of a Boolean lattice are 
	    precisely the singleton sets."},
	
	EXAMPLE {
		"L = booleanLattice 4;",
		"irreducibles L"
		},
	    
	PARA {"When L is the lattice of divisors of a non-negative integer N, the irreducible
	    elements of L are precisely the prime power divisors of N."},
	
	EXAMPLE {
		"L = divisorPoset 108;",
		"irreducibles L"
		},
	    
	PARA {"A quicker alternative version of this function exists for computing the irreducible
	    elements of the lattice of flats of a simple matroid M by exploiting the fact that a
	    flat F is irreducible if and only if the restriction of M to F is connected.
	    For the cycle matroid of a (simple) graph G, the irreducibles correspond precisely to
	    2-connected induced subgraphs of G."},
	
	EXAMPLE {
		"G = completeGraph 4;",
		"M = matroid G",
		"Irr = irreducibles M",
		"subgraphs = apply(Irr, F -> sum apply(F, i -> M.cache#(symbol groundSet)#i) )"
		},
	    	    
	SeeAlso => {elementaryDivisors, elementaryDivisorIsomorphism, factors, 
	    irreducibleFactors, irreduciblesGraph, isBuildingSet}

	    }


	
document {
	Key => {isIrreducible, (isIrreducible, Poset), (isIrreducible, Poset, Thing)},
	
	Headline => "whether a lattice or an element of a lattice is irreducible",
	
	Usage => "isIrreducible L, \n isIrreducible(L, x)",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
	    	"x" => Thing => {"an element of the lattice"},
		},
	
	Outputs => {
		Boolean => {"whether the lattice or element is irreducible"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"A poset is irreducible if it is not isomorphic to the direct product of two other
	    posets, both of which have at least two elements."},
	
	PARA {"Every Boolean lattice on at least two elements is reducible since it is isomorphic 
	    to a product of chains of length two.  The decomposition of the lattice as a product 
	    of smaller intervals in the lattice is determined by elementary divisor isomorphism of 
	    the maximal element."},
	
	EXAMPLE {
		"B = booleanLattice 3;",
		"isIrreducible B",
		"elementaryDivisorIsomorphism(B, \"111\")"
		},
	    
	PARA {"The following is an example of an irreducible lattice."},
	
	EXAMPLE {
		"P = partitionLattice 3;",
		"isIrreducible P"
		},

	PARA {"An element x of a poset P having a unique minimal element 0 is irreducible if the 
	    interval [0, x] is irreducible."},
	
	EXAMPLE {
		"P = partitionLattice 4;",
		"isIrreducible(P, {{1,2,3},{4}})"
		},
	    	    
	SeeAlso => {elementaryDivisorIsomorphism, irreducibleFactors, irreducibles, 
	    irreduciblesGraph}

	    }






