
idealGradedMoebiusAlgebra = method(
    Options => { 
	CoefficientRing => QQ,
	RingOptions => new OptionTable from {},
	Variable => "y",
	VariableOrder => null
	}
    )

idealGradedMoebiusAlgebra Matroid := Ideal => opts -> M -> (
    y := getSymbol opts.Variable;
    kk := opts.CoefficientRing;
    varList := if opts.VariableOrder === null then toList M.groundSet else opts.VariableOrder;
    S := kk[apply(varList, i -> y_i), opts.RingOptions];
    y = hashTable apply(S_*, v -> last baseName v => v);
    L := ideal apply(S_*, v -> v^2);
    C := circuits M;
    mon := I -> product apply(toList I, i -> y#i);
    C = ideal flatten apply(C, c -> apply(subsets(c, 2)/toList, p -> mon(c - {p#0}) - mon(c - {p#1}) ) );
    ideal mingens (L + C)
    )

idealGradedMoebiusAlgebra Graph := Ideal => opts -> G -> idealGradedMoebiusAlgebra(matroid G, opts)


-- GRAPH METHODS
-----------------------------------------------------------------

sunGraph = method()

sunGraph ZZ := Graph => n -> (
    E := subsets(n, 2)|flatten apply(n, i -> {{n + i, (i + 1)%n}, {n + i, i}});
    graph E 
    )

-----------------------------------------------------------------

isSimpleVertex = method()

isSimpleVertex (Graph, Thing) := Boolean => (G, v) -> (
    Nv := sort ((toList neighbors(G, v))|{v});
    NP := poset(unique apply(Nv, w -> neighbors(G, w) + set{w}), isSubset);
    C := maximalChains NP;
    #C == 1
    )


-----------------------------------------------------------------

isStronglyChordal = method()

isStronglyChordal Graph := Boolean => G -> (
    V := vertices G;
    if #V < 4 then true
    else (
    	v := select(V, w -> isSimpleVertex(G, w) );
    	if #v == 0 then false
	else (
	    v = first v;
    	    isStronglyChordal deleteVertex(G, v)
	    )
	)
    )
    

-- MATROID CHORDALITY
-----------------------------------------------------------------

isStronglyTChordal = method(Options => {Verbose => false})

isStronglyTChordal (Matroid, List) := Boolean => o -> (M, E) -> (
    if set E =!= M.groundSet then (
	error "isStronglyTChordal: Expected the list to be a permutation of the
	ground set of the matroid."
	);
    C := circuits M;
    T := select(C, c -> #c == 3);
    C = select(C, c -> #c >= 4);
    minE := s -> first select(E, e -> s#?e);
    -- find the smallest element in a set s
    
    C = hashTable apply(C, c -> c => select(T, t -> c#?(minE t) and #(t - c) == 1 ) );
    -- match circuits c of size at least 4 with 3-circuits t that contain a chord of c
    -- and the largest element of t is in c
    
    chordless := select(keys C, c -> 
	any(toList(c - {minE c}), i -> all(C#c, t -> t#?i ) )
	);
    -- checks for each circuit c whether c - {i} has a nbc-compatible triangle for each 
    -- i =!= minE c
    
    if #chordless > 0 then (
	if o.Verbose then return chordless else return false
	)
    else true     
    )


-----------------------------------------------------------------

isCChordal = method()

isCChordal Matroid := Boolean => M -> (
    C := circuits M;
    C4 := select(C, c -> #c >= 4);
    C' := select(flatten apply(#C, i -> apply(i, j -> (C#i, C#j)) ), 
	(c, d) -> #(c*d) == 1
	);
    C' = set unique apply(C', (c, d) -> c + d - c*d);
    #C4 == #(C'*(set C))
    )

-----------------------------------------------------------------

isTChordal = method()

isTChordal Matroid := Boolean => M -> (
    C := circuits M;
    C4 := select(C, c -> #c >= 4);
    C3 := select(C, c -> #c >= 3);
    all(C4, c -> any(C3, t -> #(t - c) == 1) )
    )


-----------------------------------------------------------------

brokenCircuitComplex = method()

brokenCircuitComplex (Matroid, List) := SimplicialComplex => (M, E) -> (
    if set E =!= M.groundSet then (
	error "brokenCircuitComplex: Expected the list to be a permutation of the
	ground set of the matroid."
	);
    I := ideal M;    
    S := ring I;
    minE := s -> first select(E, e -> s#?e);
    -- find the smallest element in a set s
    
    brokenCircuits := apply(circuits M, c -> c - {minE c});
    BC := monomialIdeal apply(brokenCircuits/toList, bc -> product apply(bc, i -> S_i) );
    simplicialComplex BC  
    ) 

brokenCircuitComplex Matroid := SimplicialComplex => M -> brokenCircuitComplex(M, toList M.groundSet)


-----------------------------------------------------------------

nbcBases = method()

nbcBases (Matroid, List) := List => (M, E) -> (
    nbc := facets brokenCircuitComplex(M, E);
    n := #(M.groundSet);
    apply(nbc, b -> (e := first exponents b; select(n, i -> e#i == 1) ) )
    ) 


nbcBases Matroid := List => M -> nbcBases(M, toList M.groundSet)


-----------------------------------------------------------------

rank (Poset, Thing) := (P, x) -> (
    if not isRanked P then (
	error "rank: Expected a ranked poset."
	);
    if not (set P_*)#?x then (
	error "rank: Expected an element of the poset."
	);
    rk := rankFunction P;
    rk#(position(P_*, v -> v === x))
    )


-----------------------------------------------------------------

matroid Poset := Matroid => opts -> L -> (
    if not isGeometric L then (
	error "matroid: Expected a geometric lattice."
	);
    One := first maximalElements L;
    r := rank(L, One);
    matroid apply(select(subsets(atoms L, r), b -> posetJoin(L, b) === One ),
	b -> flatten(b/toList) )
    )


-----------------------------------------------------------------

lineArrangement = method()

lineArrangement List := Poset => F -> (
    points := F/set;
    E := sum points;
    incidence := hashTable apply(toList E, l ->  l => set select(points, p -> p#?l) );
    if any(subsets(E, 2)/toList, l -> #((incidence#(l#0))*(incidence#(l#1))) > 1) then (
	error "lineArrangement: Not the points of a line arrangement."
	);
    G := {set{}}|apply(toList E, i -> set {i})|points|{E};
    poset(G, isSubset)    
    )


-- VARIABLE ORDERS
-----------------------------------------------------------------

isStrongElimOrder = method()

isStrongElimOrder (Graph, List) := (G, V) -> (
    if set vertices G =!= set V then (
	error "isStrongElimOrder: Expected the list to be a permutation of the vertices of
	the graph."
	);
    
    N := (i, v) -> select(drop(V, i), w -> (neighbors(G, v))#?w or w == v);
    -- get closed neighborhood of v in the induced subgraph G[V#i,..., V#n] 
    
    if #V == 1 then true
    else (
	all(#V - 1, i -> (
		A := N(i, V#i);
		A = flatten apply(#A, l -> apply(l, k -> {A#k, A#l}) );
		-- get all pairs of neighbors {V#k, V#l} of V#i with i < k < l 
		all(A, p -> isSubset(N(i, p#0), N(i, p#1)) ) )
            )
    	)
    )


-----------------------------------------------------------------

strongElimOrder = method()

strongElimOrder Graph := List => G -> (
    SEO := {};
    Gi := G;
    N := (K, v) -> neighbors(K, v) + set {v};
    FP := poset(vertices Gi, (v, w) -> v == w);
    i := 0;
    local simple;
    while i < #(vertices G) do (
        FP = poset(vertices Gi, (v, w) -> (
	    A := N(Gi, v); 
	    B := N(Gi, w); 
	    (isSubset(A, B) and A =!= B) or compare(FP, v, w)
	    )
	);
	simple = select(minimalElements FP, v -> isSimpleVertex(Gi, v) );
	if #simple == 0 then (
	    error "strongElimOrder: Expected a strongly chordal graph."
	    );
	SEO = SEO|{first simple};
	Gi = deleteVertex(Gi, first simple);
	i = i + 1	  
	);
    SEO
    )

posetJoin (Poset, List) := List => (P, B) -> (
    if not isSubset(B, P_*) then (
	error "posetJoin: Expected a list of elements of the ground set of the poset."
	);
    indexElement := a -> position(P.GroundSet, i -> i === a);
    upperBounds := if #B == 0 then (
	P_*/indexElement 
	) else (
	toList product apply(B, a -> set ( (principalFilter(P, a))/indexElement ) )
	);
    if upperBounds == {} then (
	error "posetJoin: The elements do not share any upper bounds."
	);
    M := P.RelationMatrix;
    heightUpperBounds := flatten apply(upperBounds, i -> sum entries M_{i});
    if #(select(heightUpperBounds, i -> i == min heightUpperBounds)) > 1 then (
	error "The join does not exist; the least upper bound is not unique."
	)
    else (
	first P.GroundSet_(upperBounds_{position (heightUpperBounds, l -> l == min heightUpperBounds)})
	)    
    )


/// EXAMPLE

G = graph {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3},{1,4},{2,4},{2,5},{3,5},{2,6},{3,6},{5,6}}

///
