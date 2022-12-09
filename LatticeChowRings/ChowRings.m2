-- BUILDING SETS
-----------------------------------------------------------------

factors = method() 

factors (Poset, List, Thing) := List => (L, G, x) -> (
   if not (set L_*)#?x then (
	error "factors: Expected the third argument to be an element
	of the ground set of the lattice input as the first argument."
	);
    maximalElements subposet(L, select(G, y -> compare(L, y, x) ) )
    )


-----------------------------------------------------------------

isBuildingSet = method ()

isBuildingSet (Poset, List) := Boolean => (L, G) -> (
    if not isLattice L then error "isBuildingSet: Expected a lattice.";
    if not isSubset(G, set L_*) then (
	error "isBuildingSet: Expected a list of elements in the lattice."
	);
    Zero := first minimalElements L;
    if (set G)#?Zero then false
    else (
	Irr := irreducibles L;
    	if not isSubset(Irr, G) then false
	else all(select(subsets(G, 2), p -> 
		not compare(L, p#0, p#1) and not compare(L, p#1, p#0) 
		and first posetMeet(L, p#0, p#1) =!= Zero ),
	        p -> (set G)#?(first posetJoin(L, p#0, p#1))
		)
	)  
    )
    
-- INPUT:  A poset L that is a lattice and a list G of nonzero elements of L.
-- OUTPUT: Whether G is a building set for L.     


-----------------------------------------------------------------

buildingSets = method()

buildingSets Poset := List => L -> (
    Zero := first minimalElements L;
    Irr := irreducibles L;
    select(subsets(L_* - set{Zero}), G -> isSubset(Irr, G) and isBuildingSet(L, G) )
    )

-- INPUT:  A poset L that is a lattice.
-- OUTPUT: A list of all building sets for L.     


-----------------------------------------------------------------

augmentedBuildingSet = method()

augmentedBuildingSet Matroid := List => M -> (
    N := freeCoextension M;
    e := max toList N.groundSet;
    apply(toList M.groundSet, i -> {i})|(select(flats N, f -> f#?e)/toList/sort)
    )


--NESTED SETS
-----------------------------------------------------------------

nestedSetComplex = method(Options => {
	CheckWellDefined => false, 
	CoefficientRing => QQ,
	Presentation => "standard",
	RingOptions => new OptionTable from {MonomialOrder => GLex},
	Variable => "x",
	VariableOrder => null
	}
    )

nestedSetComplex (Poset, List) := SimplicialComplex => o -> (L, G) -> (
    if o.CheckWellDefined and not isBuildingSet(L, G) then (
	error "nestedSetComplex: Expected the second argument to be a building set for
	the first argument."
	)
    else (
	if not isLattice L then (error "isBuildingSet: Expected a lattice as the first argument.");
    	if not isSubset(G, set L_*) then (
	    error "isBuildingSet: Expected the second argument to be a list of elements in the lattice."
	    );
	);
    NG := unique apply(maximalChains L, c -> if #c == 0 then {} else toList sum apply(c, v -> set factors(L, G, v) ) );
    x := getSymbol o.Variable;
    S := QQ[apply(G, v -> x_v)];
    x = hashTable apply(S_*, v -> last baseName v => v);
    simplicialComplex apply(NG, f -> product apply(f, v -> x#v) )
    )



-- CHOW RINGS OF ATOMIC LATTICES
-----------------------------------------------------------------

idealChowRingFY = method(Options => {
	CheckWellDefined => false,
	CoefficientRing => QQ,
	Presentation => "standard",
	RingOptions => new OptionTable from {MonomialOrder => GLex},
	Variable => "x",
	VariableOrder => null
	}
    )

idealChowRingFY (Poset, List) := opts -> (L, G) -> (
    NG := nestedSetComplex(L, G, opts);
    S := ring NG;
    x := hashTable apply(S_*, v -> last baseName v => v);
    I1 := monomialIdeal NG;
    A := atoms L;
    I2 := ideal apply(A, a -> sum apply(select(G, b -> compare(L, a, b) ), b -> x#b ) );
    N := I1 + I2;
    if opts.Presentation == "atom-free" 
    then (
	f := apply(L, a -> x#a => -sum apply(select(G, b -> compare(L, a, b) and not compare(L, b, a) ), b -> x#b ) );
	N = sub(N, f);
	kk := coefficientRing S;
	S' := kk[select(S_*, v -> not (set A)#?(last baseName v) )];
	x' := hashTable apply(S'_*, v -> last baseName v => v);
	f = map(S', S, apply(G, b -> if (set A)#?b then x#b => 0_S' else x#b => x'#b ) );
	f N
	)
    else N  
    )


-- AUGMENTED CHOW RINGS OF MATROIDS
-----------------------------------------------------------------

idealAugmentedChowRing = method(
    Options => {
	AugmentedVariable => "y",
	CoefficientRing => QQ,
	Presentation => "standard",
	RingOptions => null,
	Variable => "x", 
	VariableOrder => null
	}
    )

idealAugmentedChowRing Matroid := Ideal => o -> M -> (
    if o.Presentation == "FY" then (
	idealChowRingFY(latticeOfFlats freeCoextension M, augmentedBuildingSet M, o)
	)
    else (
	L := latticeOfFlats M;
    	A := atoms L;
    	L = if o.Presentation == "atom-free" then subposet(L, select(L_*, F -> rank(M, F) > 0) )
            else subposet(L, select(L_*, F -> rank(M, F) < rank M) );
    	y := getSymbol o.AugmentedVariable;
    	x := getSymbol o.Variable;
	kk := o.CoefficientRing;
    	if not isField kk then (
	    error "idealAugmentedChowRing: Expected the coefficient ring for the Chow ring
	    to be a field."
	    );
    	ringOpts := new OptionTable from if o.RingOptions === null then {} else o.RingOptions;
    	varList := if o.VariableOrder === null then L_*
               else (
		   if set L_* =!= set o.VariableOrder 
		   then (
		       if o.Presentation == "atom-free" then (
			   error "idealAugmentedChowRing: Expected VariableOrder to be a permutation 
		       	   of the list of flats of rank at least 1 in the matroid."
			   )
		       else (
			   error "idealAugmentedChowRing: Expected VariableOrder to be a permutation 
		       	   of the list of proper flats in the matroid."
			   )
		       )
	       	   else o.VariableOrder
		   );
    	S := if o.Presentation == "atom-free" 
	     then kk[apply(varList, F -> x_F), ringOpts]
	     else kk[apply(A, i -> y_i)|apply(varList, F -> x_F), ringOpts];
    	if o.Presentation == "atom-free" then (
	    x = hashTable apply(S_*, v -> last baseName v => v)
	    )
	else (
	    y = hashTable apply(take(S_*, #A), v -> last baseName v => v);
    	    x = hashTable apply(drop(S_*, #A), v -> last baseName v => v)
	    );
    	incomp := select(subsets(L_*, 2), 
	    p -> not compare(L, p#0, p#1) and not compare(L, p#1, p#0) );
    	I1 := ideal apply(incomp, F -> x#(F#0)*x#(F#1) );
    	I2 := ideal flatten apply(A, a -> apply(select(L_*, F -> not compare(L, a, F) ), 
	    F -> if o.Presentation == "atom-free" 
	         then x#F*(sum apply(select(L_*, G -> compare(L, a, G) and compare(L, F, G) ), G -> x#G ) ) 
	         else y#a*x#F) );
    	 beta := a -> sum apply(select(L_*, F -> not compare(L, a, F) ), F -> x#F );
    	 I3 := ideal apply(A, a -> 
	     if o.Presentation == "atom-free" 
	     then (l := sum apply(select(L_*, G -> compare(L, a, G)), G -> x#G ); l^2) 
	     else y#a - beta a);
    	 I1 + I2 + I3
	 )
    )

-- INPUT:  A matroid M.
-- OUTPUT: The defining ideal of the augmented Chow ring of M.
-- OPTION: Variable => Specifies the letter used for the variables in the ambient
--                     polynomial ring.
--    	   Presentation => "atom-free" => Obtained from Feichtner-Yuzvinsky presentation 
--                          after suitable change of variables and killing the variables
--                          corresponding to atoms in the lattice of flats.
--    	      	      	=> "FY" => The Feichtner-Yuzvinsky presentation for the lattice
--    	      	      	    of flats of the free coextension with respect to a non-maximal
--    	      	      	    building set.






-- CHOW RING KOSZUL FILTRATION
-----------------------------------------------------------------

matroidChowRingFiltration = method(
    Options => {
	Augmented => false,
	AugmentedVariable => "y",
	CoefficientRing => QQ,
	Presentation => "AtomFree",
	Variable => "x", 
	VariableOrder => null,
	Verbosity => 1
	}
    )

matroidChowRingFiltration Matroid := HashTable => o -> M -> (
    I := if o.Augmented 
         then idealAugmentedChowRing(M, o)
	 else idealChowRing(M, o);
    S := ring I;
    R := S/I;
    x := hashTable apply(R_*, v -> last baseName v => v);
    L := latticeOfFlats M;
    L = if o.Augmented 
        then subposet(L, select(L_*, f -> rank(M, f) > 0))
	else subposet(L, select(L_*, f -> rank(M, f) >= 2));
    U := A -> if #A > 0 then ideal apply(A, f -> x#f) else ideal(0_R);
    F0 := apply(select(antichains L, A -> #A > 0), A -> (
	    G := filter(L, A);
	    if o.Verbosity >= 2 then (
		print("Finding filter for: ");
		print A
		);
	    U(G) => U(G - set {first A}) )
	);
    L' := if o.Augmented 
          then select(L_*, f -> rank(M, f) >= 2)
	  else select(L_*, f -> rank(M, f) >= 3);
    tco := totalCoatomOrder(M, Augmented => o.Augmented);
    F1 := flatten apply(L', f -> (
	    C := select(L_*, g -> not isSubset(g, f) );
	    hyp := select(tco, g -> (set coatoms (L, f))#?g);
	    apply(#hyp, i -> U(C|drop(hyp, -i)) => U(C|drop(hyp, -i -1)) )
	    )
	);
    hashTable(F0|F1)
    )

matroidChowRingFiltration (Matroid, List) := HashTable => o -> (M, G) -> (
    L := latticeOfFlats M;
    I := idealChowRingFY(L, G);
    S := ring I;
    R := S/I;
    x := hashTable apply(R_*, v -> last baseName v => v);
    G2 := subposet(L, select(G, f -> rank(M, f) >= 2));
    U := A -> if #A > 0 then ideal apply(A, f -> x#f) else ideal(0_R);
    F0 := apply(select(antichains G2, A -> #A > 0), A -> (
	    FG := filter(G2, A);
	    if o.Verbosity >= 2 then (
		print("Finding filter for: ");
		print A
		);
	    FG' := FG - set {first A};
	    U(FG/sort) => U(FG'/sort) )
	);
    G3 := select(G, f -> rank(M, f) >= 3);
    MS := (first maximalChains totalCoatomOrder(M, G))/toList;
    F1 := flatten apply(G3, f -> (
	    C := select(G2_*, g -> not isSubset(g, f) );
	    hyp := select(MS, g -> (set coatoms (f, G2))#?g);
	    apply(#hyp, i -> U((C|drop(hyp, -i))/sort) => U((C|drop(hyp, -i -1))/sort) )
	    )
	);
    hashTable(F0|F1)
    )


