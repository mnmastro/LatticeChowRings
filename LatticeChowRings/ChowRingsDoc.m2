document { 
	Key => LatticeChowRings,
	Headline => "a package for working with Chow rings of atomic lattices",
	EM "LatticeChowRings", " is a package for working with the Chow rings of smooth 
	toric varieties associated to finite atomic lattices by Feichtner and Yuzvinsky as a
	generalization of the De Concini-Procesi wonderful compactifications of a hyperplane
	arrangement.",
	
	PARA {
	    "Given an arrangement of (linear) hyperplanes H_1,...,H_n in a vector space V, the 
	    De Concini-Procesi wonderful compactification of the complement of the arrangement
	    is..."
	    }
	}

document {
	Key => {augmentedBuildingSet, (augmentedBuildingSet, Matroid)},
	
	Headline => "building set of the augmented Chow ring of a matroid",
	
	Usage => "augmentedBuildingSet M",
	
	Inputs => {
	    	"M" => Matroid
		},
	
	Outputs => {
		List => {"the building set of the augmented Chow ring of the matroid"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"The augmented Chow ring of a (simple) matroid M is the Feichtner-Yuzvinsky Chow ring
	    of the lattice of flats of another matroid N, called the free coextension of M,
	    with respect to a certain building set.  This function produces that building set."},
	
	PARA {"The free coextension N is a matroid whose ground set is the ground set of M plus one
	    additional element e.  Flats of the free coextension come in one of two types: They are 
	    either independent sets of M or sets containing e that become flats of M after removing e.  
	    The building set for the augmented Chow ring consists of all flats of the free coextension
	    N that are of the second type above together with all independent sets of rank one."},
	
	EXAMPLE {
		"M = uniformMatroid(3, 4);",
		"N = coextension M;",
		"L = latticeOfFlats N;",
		"G = augmentedBuildingSet M",
		"isBuildingSet(L, G)"
		},
	        
	SeeAlso => {coextension, isBuildingSet}
	    }

document {
	Key => {AugmentedVariable},
	
	Headline => "an optional argument",
	
	PARA {"A symbol used as the name of an optional argument."},

	    }
	
document {
	Key => {buildingSets, (buildingSets, Poset)},
	
	Headline => "all building sets of a lattice",
	
	Usage => "buildingSets L",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
		},
	
	Outputs => {
		List => {"of all building sets of the lattice"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"If L is a finite lattice with minimal element 0, a set G of non-minimal elements 
	    of L is a building set for L if intervals below elements in G can be used to recover 
	    the order structure for the entire lattice.  Specifically, for every non-minimal 
	    element x of L, if x_1,..., x_n denote the maximal elements of G less than or equal to x
	    there is a poset isomorphism from the product of the intervals [0, x_j] for j = 1,..., n 
	    to the interval [0, x] that sends the element (0,...,0, x_j,0,...,0) of the product to
	    the element x_j for each j.  This function finds all possible building sets for a given
	    lattice L."},
	
	EXAMPLE {
		"B = booleanLattice 3;",
		"BS = buildingSets B",
		"all(BS, G -> isBuildingSet(B, G) )"
		},
		    
	SeeAlso => {isBuildingSet, irreducibles}
	    }		

document {
	Key => {factors, (factors, Poset, List, Thing)},
	
	Headline => "of a lattice element with respect to a building set",
	
	Usage => "factors(L, G, x)",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
	    	"G" => List => {"a list of elements of the lattice forming a building set"},
		"x" => Thing => {"an element of the lattice"},
		},
	
	Outputs => {
		List => {"of factors of the lattice element with respect to the building set"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"The list of factors of an element x of a lattice L with respect to a building
	    set G are the maximal elements of G that are less than or equal to x."},
	
	EXAMPLE {
		"L = booleanLattice 4;",
		"G = irreducibles L",
		"factors(L, G, \"0110\")"
		},
	    
	PARA {"When G is the minimal building set of all irreducible elements of L, the factors
	    of an element x are called the elementary divisors of x.  In general, a set G of 
	    non-minimal elements of L is building if and only if it contains the irreducibles and
	    for each non-minimal x in L, the sets of elementary divisors of the factors of x with
	    respect to G partition the set of elementary divisors of x."},
	    
	EXAMPLE {
		"D = divisorPoset 300;",
		"Irr = irreducibles D;",
		"G = Irr|{10, 20, 50, 100}",
		"isBuildingSet(D, G)",
		"F = factors(D, G, 300)",
		"elementaryDivisors(D, 300)",
		"apply(F, x -> elementaryDivisors(D, x))",
		},
	        
	Caveat => {"This function does not check whether L is a lattice or G is a building set
	    in L since it is usually called by methods such as ", TO isBuildingSet, " and ", 
	    TO nestedSetComplex, " which can perform such checks."
	    }, 
		
	SeeAlso => {isBuildingSet, elementaryDivisors}
	    }	

document {
	Key => {[idealAugmentedChowRing, AugmentedVariable]},
	
	Headline => "set the base letter of the augmented variables in the ambient polynomial ring",
	
	Usage => "idealAugmentedChowRing(M, AugmentedVariable => \"q\")",
	
	Inputs => {
	    	"M" => Matroid,
		},
	
	Outputs => {
		Ideal => {"the defining ideal of the augmented Chow ring of the matroid"}
		},
	
	PARA {"This option is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Use this option to specify the base letter of the augmented variables 
	    in the polynomial ring for the defining ideal of the augmented Chow ring that
	    correspond to rank-one flats of the matroid."},
	
	EXAMPLE {
		"M = uniformMatroid(3, 3)",
		"I = idealAugmentedChowRing(M, AugmentedVariable => \"q\")",
		},
	    
	PARA {"In the standard presentation of the augmented Chow ring, there are 
	    additional variables corresponding to the all proper flats of 
	    the matroid.  The base letter of these variables can also be changed
	    using the option ", TO Variable, "."},
	    	    
	SeeAlso => {idealAugmentedChowRing, [idealAugmentedChowRing, Variable]}

	    }	

document {
	Key => {[idealAugmentedChowRing, CoefficientRing]},
	
	Headline => "specify the coefficient field of the augmented Chow ring",
	
	Usage => "idealAugmentedChowRing(M, CoefficientRing => kk)",
	
	Inputs => {
	    	"M" => Matroid,
		},
	
	Outputs => {
		Ideal => {"the defining ideal of the augmented Chow ring of the matroid"}
		},
	
	PARA {"This option is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Use this option to specify the coefficient field kk of the polynomial ring
	    for the defining ideal of the augmented Chow ring."},
	
	EXAMPLE {
		"M = uniformMatroid(2, 3)",
		"I = idealAugmentedChowRing(M, CoefficientRing => ZZ/7)",
		},
	    	    
	SeeAlso => {idealAugmentedChowRing}

	    }

document {
	Key => {[idealAugmentedChowRing, RingOptions]},
	
	Headline => "pass options to the ambient polynomial ring",
	
	Usage => "idealAugmentedChowRing(M, RingOptions => opts)",
	
	Inputs => {
	    	"M" => Matroid,
		},
	
	Outputs => {
		Ideal => {"the defining ideal of the augmented Chow ring of the matroid"}
		},
	
	PARA {"This option is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Use this option to pass a list of ring-specific options such as 
	    monomial orders, weights, etc. to the ambient polynomial ring of the defining 
	    ideal of the augmented Chow ring."},
	
	EXAMPLE {
		"M = uniformMatroid(2, 3)",
		"I = idealAugmentedChowRing(M, RingOptions => {MonomialOrder => Lex}, Presentation => \"AtomFree\")",
		"apply(I_*, f -> leadTerm f)",
		},
	    	    
	SeeAlso => {idealAugmentedChowRing}

	    }
	
document {
	Key => {[idealAugmentedChowRing, Variable]},
	
	Headline => "set the base letter of the variables in the ambient polynomial ring",
	
	Usage => "idealAugmentedChowRing(M, Variable => \"a\")",
	
	Inputs => {
	    	"M" => Matroid,
		},
	
	Outputs => {
		Ideal => {"the defining ideal of the augmented Chow ring of the matroid"}
		},
	
	PARA {"This option is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Use this option to specify the base letter of the variables in the 
	    polynomial ring for the defining ideal of the augmented Chow ring that
	    correspond to flats of the matroid."},
	
	EXAMPLE {
		"M = uniformMatroid(3, 3)",
		"I = idealAugmentedChowRing(M, Variable => \"a\")",
		},
	    
	PARA {"In the standard presentation of the augmented Chow ring, there are 
	    additional augmented variables corresponding to the rank-one flats of 
	    the matroid.  The base letter of these variables can also be changed
	    using the option ", TO AugmentedVariable, "."},
	    	    
	SeeAlso => {idealAugmentedChowRing, [idealAugmentedChowRing, AugmentedVariable]}

	    }	
	
document {
	Key => {idealChowRingFY, (idealChowRingFY, Poset, List)},
	
	Headline => "the defining ideal of the Chow ring of an atomic lattice",
	
	Usage => "idealChowRingFY(L, G)",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
	    	"G" => List => {"a list of elements of the lattice forming a building set"},
		CheckWellDefined => Boolean,
		CoefficientRing => Ring,
		Presentation => String,
		RingOptions => OptionTable,
		Variable => String,
		VariableOrder => List,
		},
	
	Outputs => {
		Ideal => {"the  defining ideal of the Chow ring of the lattice with respect to the building set"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"The Chow ring of a finite, atomic lattice L and a building set G for L is the Chow ring
	    of a smooth toric variety associated to L and G by Feichtner and Yuzvinsky.  This function 
	    constructs the defining ideal for a presentation of that ring.  In a polynomial ring with variables indexed
	    by the elements of G, we associate to each atom a of L the linear form that is the sum of 
	    all variables indexed by elements of G that are greater than or equal to a.  The defining ideal 
	    of the Chow ring is then the sum of the Stanley-Reisner ideal of the simplicial complex of 
	    G-nested subsets of L together with the ideal generated by the aforementioned linear forms 
	    for all atoms of L."},
	
	PARA {"When L is the lattice of flats of a matroid and G is the building set of all nonempty
	    flats of the matroid, we recover the defining ideal for a presentation of the Chow ring
	    of the matroid.  The Chow ring of a matroid is well known to satisfy Poincar\u00E9 duality, and so,
	    it is always a Gorenstein ring.  The following example shows that this is not necessarily the case
	    for Chow rings of non-geometric lattices."},
	
	EXAMPLE {
	    	"V = {0,a,b,c,d,e,f,1}",
		"H = {{0,a},{0,b},{0,c},{0,d},{a,e},{b,e},{c,f},{d,f},{e,1},{f,1}};",
		"L = poset(V, H)", 
		"(isAtomic L) and not (isUpperSemimodular L)",
		"I = idealChowRingFY(L, drop(V, 1))",
		"numerator hilbertSeries(I, Reduce => true)"
		},
	        
	SeeAlso => {isBuildingSet, nestedSetComplex}
	    }
	

document {
	Key => {isBuildingSet, (isBuildingSet, Poset, List)},
	
	Headline => "whether a list of elements of a lattice is a building set",
	
	Usage => "isBuildingSet(L, G)",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
	    	"G" => List => {"a list of elements of the lattice"},
		},
	
	Outputs => {
		Boolean => {"whether the list of elements is a building set"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"If L is a finite lattice with minimal element 0, a set G of non-minimal elements 
	    of L is a building set for L if intervals below elements in G can be used to recover 
	    the order structure for the entire lattice.  Specifically, for every non-minimal 
	    element x of L, if x_1,..., x_n denote the maximal elements of G less than or equal to x
	    there is a poset isomorphism from the product of the intervals [0, x_j] for j = 1,..., n 
	    to the interval [0, x] that sends the element (0,...,0, x_j,0,...,0) of the product to
	    the element x_j for each j."},
	
	PARA {"For any lattice L, the set consisting of all elements in the lattice
	    except its minimal element 0 is always a building set, in which case the required
	    poset isomorphism is just the identity map on [0, x].  It is the unique largest possible 
	    building set."},
	
	EXAMPLE {
		"L = latticeOfFlats uniformMatroid(3, 3);",
		"Zero = first minimalElements L",
		"G = delete(Zero, L_*)",
		"isBuildingSet(L, G)"
		},
	    
	PARA {"For any lattice L, the set consisting of all irreducible elements in the lattice
	    is also always a building set.  It is the unique smallest possible building set.  See ", 
	    TO irreducibles, " for more information on irreducible elements."},
	
	EXAMPLE {
		"L = latticeOfFlats matroid completeGraph 4;",
		"G = irreducibles L",
		"isBuildingSet(L, G)"
		},

	PARA {"Not all subsets of non-minimal elements containing the irreducible elements of a 
	    lattice may by building."},
	
	EXAMPLE {
		"L = latticeOfFlats uniformMatroid(4, 4);",
		"G = select(L_*, f -> #f == 1 or #f == 2)",
		"isSubset(set irreducibles L, G)",
		"isBuildingSet(L, G)"
		},
	 
	PARA {"One reason that the above list of flats of the uniform matroid U_{4, 4} fails to 
	    be building is that the flats {1, 3} and {2, 3} have nonempty intersection, but their 
	    join {1, 2, 3} does not appear in the list.  Building sets can be equivalently 
	    characterized as precisely the sets G of non-minimal elements of L that contain the 
	    irreducible elements and have the property that for any two incomparable elements of 
	    G with non-minimal meet, their join is also in G.  This is how the function ", 
	    TO isBuildingSet, " checks whether a set is building in practice."}, 
	    
	SeeAlso => {buildingSets, factors, irreducibles}
	    }

document {
	Key => {nestedSetComplex, (nestedSetComplex, Poset, List)},
	
	Headline => "the simplicial complex of nested sets of a building set",
	
	Usage => "nestedSetComplex(L, G)",
	
	Inputs => {
	    	"L" => Poset => {"a finite lattice"},
	    	"G" => List => {"a list of elements of the lattice forming a building set"},
		CheckWellDefined => Boolean,
		Variable => String,
		},
	
	Outputs => {
		SimplicialComplex => {"the simplicial complex of nested sets with respect to the building set"}
		},
	
	PARA {"This function is provided by the package ", TO LatticeChowRings,"."},
	
	PARA {"Given a building set G for a lattice L, a subset N of G is nested if for any pairwise
	    incomparable elements x_1,...,x_t in N with t >= 2, the join of the x_i does not belong to G.  
	    Every nested set is obtained by unioning the set factors in G of each element of a chain in 
	    the lattice L, and so, the facets of the nested set complex are determined by applying this
	    construction to the set of maximal chains in L."},
	
	PARA {"When G is the unique maximal building set consisting of all non-minimal elements of the 
	    lattice, the set of G-factors of an element x of the lattice is precisely {x}, so the nested 
	    set complex is exactly the order complex of the lattice.  In this case, the corresponding 
	    Stanley-Reisner ideal is always generated by quadratic monomials corresponding to pairs of
	    incomparable elements."},
	
	EXAMPLE {
		"L = booleanLattice 3;",
		"Zero = first minimalElements L",
		"G = delete(Zero, L_*)",
		"D = nestedSetComplex(L, G)",
		"monomialIdeal D"
		},
	    
	PARA {"However, in general, the Stanley-Reisner ideal of a nested set complex need not be generated 
	    by quadratic monomials."},
	
	EXAMPLE {
		"G ={\"001\", \"010\", \"100\", \"111\"}",
		"isBuildingSet(L, G)",
		"D = nestedSetComplex(L, G)",
		"monomialIdeal D"
		},
	        
	SeeAlso => {factors, isBuildingSet}
	    }

document {
	Key => {RingOptions},
	
	Headline => "an optional argument",
	
	PARA {"A symbol used as the name of an optional argument."},

	    }	
