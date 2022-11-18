needsPackage "Posets"

coatoms = method()

coatoms Poset := List => P -> (
    maxRels := select(coveringRelations P, R -> any(maximalElements P, m -> m === R#1) );
    unique apply(maxRels, R -> R#0)
    )

coatoms (Poset, Thing) := List => (P, x) -> apply(select(coveringRelations P, R -> x === R#1), R -> R#0)


-- POSET MAPS
-----------------------------------------------------------------

PosetMap = new Type of HashTable

net PosetMap := Net => P -> (
     H := P.map;
     horizontalJoin flatten (
	 net class P,
         "{",
         stack (horizontalJoin \ sort apply(pairs H, (k,v) -> (net k, " => ", net v))),
	"}"
	)
    )


----------------------------------------

posetMap = method()

posetMap (Poset, Poset, List) := PosetMap => (Q, P, f) -> (
    new PosetMap from {
	cache => new CacheTable from {},
	(symbol target) => Q,
	(symbol source) => P,
	(symbol map) => hashTable f
	}
    )

-- INPUT:  A source poset P, a target poset Q, and a list f of 
--    	   key-value pairs for a hashtable where the keys are elements
--    	   of P and the values of elements of Q.
-- OUTPUT: The PosetMap corresponding to the given data.


----------------------------------------

source PosetMap := Poset => f -> f.source
target PosetMap := Poset => f -> f.target

PosetMap Thing := Thing => (f, x) -> (
    if not (f.map)#?x then (
	error "Expected the second argument to be an element of the source of the poset map."
	); 
    (f.map)#x
    )

-- INPUT:  A source PosetMap f and an element x of the source poset of f.
-- OUTPUT: The image of x under f.


----------------------------------------

isomorphism (Poset, Poset, List) := HashTable => (P, Q, f) -> (
    	   if not all(f, kv -> instance(kv, List) and #kv == 2) and 
	   not all(f, kv -> instance(kv, Option)) then (
		   error "isomorphism: Expected the last argument to be a list of 2 element lists
		   or key-value pairs for a hashtable."
		   );
	   isoCand := hashTable f;
	   if not isSubset(keys isoCand, vertices P) then (
	       error "isomorphism: Expected the first elements of the key-value pairs in the
	       last argument to be elements of the first poset."
	       );
	   if not isSubset(values isoCand, vertices Q) then (
	       error "isomorphism: Expected the second elements of the key-value pairs in the
	       last argument to be elements of the second poset."
	       );
	   isoCand = applyPairs(isoCand, (k, v) -> (position(P_*, k' -> set {k'} === set {k}), position(Q_*, v' -> set {v'} === set {v}) ) );
           -- Test for a quick bail-out (also puts the covering relations in the cache).
           if #P_* != #Q_* or #coveringRelations P != #coveringRelations Q then (
	       error "isomorphism: The given posets are not isomorphic."
	       );
	   -- Check that all assignments have the same covering relations if both elements in the
	   -- relation are assigned.
	   if not isSubset(apply(select(P.cache.coveringRelations, 
		       c -> member(first c, keys isoCand) and member(last c, keys isoCand) ), 
		   c -> {isoCand#(first c), isoCand#(last c)}), 
	       Q.cache.coveringRelations) then (
	       error "isomorphism: There is no poset isomorphism with given assignments."
	       ); 
           -- Partition the vertices based on (#leq, #geq, #covering, #coveredBy).
           vertexPartition := P -> (
               leq := sum \ entries transpose P.RelationMatrix;
               geq := sum \ entries P.RelationMatrix;
               cr := transpose P.cache.coveringRelations;
               if #cr == 0 then cr = {{}};
               covering := tally last cr;
               coveredBy := tally first cr;
               partition(i -> {leq_i, geq_i, if covering#?i then covering#i else 0, if coveredBy#?i then coveredBy#i else 0}, 0..<#P_*)
               );
           vpP := vertexPartition P;
           vpQ := vertexPartition Q;
	   -- Check that all assignments have the same vertex partition data.
	   if not all(pairs isoCand, (k, v) -> (
		   select(keys vpP, d -> (set vpP#d)#?k) == select(keys vpQ, d -> (set vpQ#d)#?v)
		   ) ) then (
		   error "isomorphism: There is no poset isomorphism with given assignments."
		   );	       
           -- Check for compatible vertex partitions.
           if sort keys vpP != sort keys vpQ or any(keys vpP, k -> #vpP#k != #vpQ#k) then return null;
           -- This method attempts to find an isomorphism by considering permutations of each part, one at a time.
           buildIsoCand := (vpK, isoCand) -> (
               -- If vpK is empty, then we've found an isomorphism!
               if #vpK == 0 then return isoCand;
               for p in permutations vpK_0_0 do (
                   isoCand' := merge(isoCand, 
		       new HashTable from apply(vpK_0_0, 
			   i -> (vpP#(vpK_0_1))#i => (vpQ#(vpK_0_1))#(p#i)), first);
                   -- Check that the restricted covering relations of P are covering relations of Q.
                   restrictedCRP := select(P.cache.coveringRelations, c -> member(first c, keys isoCand') and member(last c, keys isoCand'));
                   isomorph := if isSubset(apply(restrictedCRP, c -> {isoCand'#(first c), isoCand'#(last c)}), Q.cache.coveringRelations) then buildIsoCand(drop(vpK, 1), isoCand');
                   -- If we found one, then return it.
                   if isomorph =!= null then return isomorph;
                   );
               );
           -- Isolated vertices (key {1,1,0,0}) can be mapped to each other in any order.
           isoCand = merge(isoCand, 
	       new HashTable from apply(if vpP#?{1,1,0,0} then #vpP#{1,1,0,0} else 0, 
		   i -> (vpP#{1,1,0,0})_i => (vpQ#{1,1,0,0})_i), first);
           iso := buildIsoCand(sort apply(keys vpP - set {{1,1,0,0}}, k -> {#vpP#k, k}), isoCand);
           -- If iso is non-null, then it is an isomorphism on the indices.  Make it an isomorphism on the vertices.
           if iso =!= null then new HashTable from apply(keys iso, k -> P_k => Q_(iso#k))
           )

-- INPUT:  A pair of isomorphic posets P and Q, and list f of 
--     	   either 2-element lists or key-value pairs for a hashtable
--    	   representing prespecified assignments that the isomorphism
--    	   should satisfy.
-- OUTPUT: A hashtable representing an isomorphism from P to Q satisfying
--    	   the assignments specified by f.


--IRREDUCIBLE POSETS
-----------------------------------------------------------------

irreduciblesGraph = method()

irreduciblesGraph Poset := Graph  => L -> (
    if not isLattice L then (error "irreduciblesGraph: Expected a lattice as input.");
    One := first maximalElements L;
    Zero := first minimalElements L;
    MI := delete(One, meetIrreducibles L);
    JI := delete(Zero, joinIrreducibles L);
    E := delete({}, flatten apply(MI, 
	    m -> apply(JI, 
		j -> if not compare(L, j, m) then {(j, 0), (m, 1)} else {}
		)
	    )
	);
    JI = apply(JI, j -> (j, 0));
    MI = apply(MI, m -> (m, 1));
    graph(JI|MI, E, EntryMode => "edges")
    )

-- INPUT:  A poset L that is a lattice.
-- OUTPUT: A bipartite graph whose bottom row consist of all
--    	   meet irreducible elements of L and whose top row
--    	   consists of all join irreducible elements with
--    	   edges between the rows if and only if a meet and
--    	   join irreducible are not comparable.
-- NOTE:   It seems the join irreducibles need to be above
--    	   the meet irreducibles to get elementaryDivisorIsomorphism
--    	   to work.  This is the reverse of Markowsky's convention.


----------------------------------------

isIrreducible = method()

isIrreducible Poset := Boolean => L -> isConnected irreduciblesGraph L

-- INPUT:  A poset L that is a lattice.
-- OUTPUT: Whether L is irreducible in the sense that it is not
--    	   a direct product of two smaller posets with at least
--    	   two elements each.  According to Markowsky, L is
--    	   irreducible if and only if its bipartite graph is 
--    	   connected.

isIrreducible (Poset, Thing) := Boolean => (L, x) -> isIrreducible subposet(L, orderIdeal(L, {x}))

----------------------------------------

irreducibles = method()

irreducibles Poset := List => L -> (
    if L.cache#?(symbol irreducibles) and L.cache#(symbol completeIrreducibleList) 
    then L.cache#(symbol irreducibles)
    else (
	Zero := minimalElements L;
	if #Zero =!= 1 then error "irreducibles: Expected the poset to have a unique minimal element.";
	Zero = first Zero;
	toRemove := if L.cache#?(symbol irreducibles) 
	           then (L.cache#(symbol irreducibles))|{Zero} 
	           else {Zero};
    	Irr := select(L_* - set toRemove, x -> isIrreducible(L, x) ) ;
	L.cache#(symbol irreducibles) = if L.cache#?(symbol irreducibles) 
	           then L.cache#(symbol irreducibles)|Irr
		   else Irr;
	L.cache#(symbol completeIrreducibleList) = true;
	L.cache#(symbol irreducibles)
	)  
    )

-- INPUT:  A poset L that is a lattice.
-- OUTPUT: The list of all non-minimal elements x of L such that the sublattice
--    	   [0, x] is irreducible.


irreducibles Matroid := List => M -> (
    if not isSimple M then error "irreducibles: Expected a simple matroid.";
    select(flats M, F -> #F > 0 and isConnected(M|F))/toList/sort
    )

----------------------------------------

irreducibleFactors = method(Options => {Output => "Posets"})

irreducibleFactors Poset := List => o -> L -> (
    if #(vertices L) < 4 then return {L};
    G := irreduciblesGraph L;
    F := connectedComponents G;
    if o.Output == "Components" then return apply(F, C -> inducedSubgraph(G, C) );
    if #F == 1 then return {L};
    F = apply(F, C -> 
	apply(select(C, v -> last v == 0), v -> neighbors(G, v)/first ) 
	);
    generatePoset := C -> (
	A := unique apply(subsets(#C), S -> if #S > 0 then toList sum apply(S, i -> C#i) else {});
	poset(A, isSubset)
	);
    apply(F, C -> generatePoset C)
    )

-- INPUT:  A poset L that is a lattice.
-- OUTPUT: The list of all irreducible factors of L as
--    	   direct product of smaller posets, output
--    	   as connected components of the bipartite graph
--    	   of L or the corresponding posets.


----------------------------------------

elementaryDivisors = method()


elementaryDivisors (Poset, Thing) := List => (L, x) -> (
    if not (set L_*)#?x then (
	error "elementaryDivisors: Expected an element of the ground set of the poset."
	);
    if L.cache#?(symbol irreducibles) and L.cache#(symbol completeIrreducibleList)
    then maximalElements subposet(L, select(L.cache#(symbol irreducibles), y -> compare(L, y, x) ) )
    else (
	D := irreducibles subposet(L, orderIdeal(L, {x}) );
	L.cache#(symbol irreducibles) = if L.cache#?(symbol irreducibles) 
	     then unique( (L.cache#(symbol irreducibles) )|D )
	     else D;
	L.cache#(symbol completeIrreducibleList) = false;
	maximalElements subposet(L, D)
	)
    )

-- INPUT:  A poset L that is a lattice and an element x of L.
-- OUTPUT: The list of all elementary divisors of x.  These are
--    	   the maximal elements of the irreducibles of L that are 
--    	   less than or equal to x.


----------------------------------------

elementaryDivisorIsomorphism = method()

elementaryDivisorIsomorphism (Poset, Thing) := PosetMap => (L, x) -> (
    D := elementaryDivisors(L, x);
    I := subposet(L, orderIdeal(L, {x}) );
    F := apply(D, y -> subposet(L, orderIdeal(L, {y}) ) );
    if #F == 1 then (
	posetMap(F#0, F#0, apply(vertices F#0, v -> v => v) )
	)
    else (
	P := productPoset F;
    	Zeros := first minimalElements P;
    	Ones := first maximalElements P;
    	f := isomorphism(I, P, apply(#Ones, 
	    	i -> Ones#i => apply(#Ones, j -> if i == j then Ones#j else Zeros#j )
	    	)
	    );
    	pos := apply(D, y -> position(Ones, v -> v == y) );
    	if any(#D, i -> f#(D#i) =!= apply(#D, j -> if j == pos#i then D#i else Zeros#j) ) then (
	    print "warning: isomorphism is not the elementary divisor isomorphism."
	    );
    	posetMap(I, P, ((pairs f)/reverse))
	)
    )

-- INPUT:  A poset L that is a lattice and an element x of L.
-- OUTPUT: The PosetMap f from the product of all irreducible subintervals
--    	   of the interval [0, x] to the interval [0, x] with the property 
--    	   that for each elementary divisor y_i of x we have f(0,..., y_i,...,0) = y_i.
-- CAVEAT: Using the built-in isomorphism method of the Posets package does not
--    	   always produce the correct isomorphism. Need to modify this method to allow
--    	   specifying certain values of the isomorphism. 


----------------------------------------

productPoset = method ()

productPoset List := Poset => F -> (
    if #F == 1 then return first F;
    verts := F/vertices;
    f := (l, l') -> flatten apply(l, i -> apply(l', j -> (i, j) ) );
    G := fold(f, verts)/deepSplice/toList;
    cmp := (u, v) -> all(#F, i -> compare(F#i, u#i, v#i) );
    poset(G, cmp)
    )

-- INPUT:  A list F of posets, the factors of the product.
-- OUTPUT: The poset which is the product all the posets in F.
-- NOTE:   This has the same effect as applying product to a list of posets,
--    	   except that elements of the ground set are represented as a single list
--    	   instead of heavily nested lists. 


-- TOTAL COATOM ORDERS
-----------------------------------------------------------------

totalCoatomOrder = method(Options => {Augmented => false})

totalCoatomOrder Matroid := Poset => o -> M -> (
    L := if o.Augmented then select(flats M, f -> rank(M, f) > 0) 
         else select(flats M, f -> rank(M, f) >= 2);
    tco := (f, g) -> (
	if rank(M, f) < rank(M, g) then  true
	else if rank(M, f) == rank(M, g) then (
	    if isSubset(f, g) and isSubset(g, f) then true 
	    else (
	    	j := max toList(f + g - f*g);
	    	f#?j
		)
	    )
	else false
	);
    (first maximalChains poset(L, tco) )/toList/sort
    )

totalCoatomOrder (Matroid, List) := Poset => (M, G) -> (
    tco := (f, g) -> (
	if rank(M, f) < rank(M, g) then  true
	else if rank(M, f) == rank(M, g) then (
	    if isSubset(f, g) and isSubset(g, f) then true 
	    else (
		f' := set f;
		g' := set g;
	    	j := max toList(f' + g' - f'*g');
	    	f'#?j
		)
	    )
	else false
	);
    (first maximalChains poset(G, tco) )/toList
    )
