
-- TEST #1
-----------------------------------------------------------------
TEST ///

B = booleanLattice 4
Irr = irreducibles B
assert(set Irr === set {"1000", "0100", "0010", "0001"})
assert isBuildingSet(B, B_* - set{"0000"})
N = nestedSetComplex
assert isBuildingSet(B, Irr)
N = nestedSetComplex(B, Irr)
S = ring N
assert(monomialIdeal N == monomialIdeal 0_S)
G = Irr|{"1100", "0110", "0011", "1110", "0111"}
assert not isBuildingSet(B, G)
G = G|{"1111"}
N = nestedSetComplex(B, G)
S = ring N
x = hashTable apply(S_*, v -> last baseName v => v)
assert(monomialIdeal N == monomialIdeal(
	x#"1000"*x#"0100", x#"1000"*x#"0110", x#"1000"*x#"0111",
	x#"0100"*x#"0010", x#"0100"*x#"0011",
	x#"0010"*x#"0001", x#"0010"*x#"1100",
	x#"0001"*x#"0110", x#"0001"*x#"1110",
	x#"1100"*x#"0110", x#"1100"*x#"0011", x#"1100"*x#"0111",
	x#"0110"*x#"0011",
	x#"0011"*x#"1110",
	x#"1110"*x#"0111"
	) )
///


-- TEST #2
-----------------------------------------------------------------
TEST ///

M = matroid completeGraph 4
L = latticeOfFlats M
Irr = irreducibles L
assert(set Irr === set irreducibles M)
assert isBuildingSet(L, L_* - set{{}})
assert isBuildingSet(L, Irr)
N = nestedSetComplex(L, Irr)
S = ring N
x = hashTable apply(S_*, v -> last baseName v => v)
cliquePairs = select(subsets(select(subsets 4, s -> #s >= 2), 2), 
	    p -> not isSubset(p#0, p#1) and not isSubset(p#1, p#0) and #(product(p/set)) > 0 )
cliquePairs = apply(cliquePairs, p -> 
    apply(p, c -> select(toList M.groundSet, i -> isSubset(M.cache#groundSet#i, set c) ) ) )
assert(monomialIdeal N == monomialIdeal apply(cliquePairs, p -> x#(p#0)*x#(p#1) ) )
G = Irr|{{0,5}, {1,4}}
assert isBuildingSet(L, G)

///


-- TEST #3
-----------------------------------------------------------------
TEST ///

D = divisorPoset 300
Irr = irreducibles D
assert(set Irr === set {2, 3, 4, 5, 25})
assert isBuildingSet(D, D_* - set{1})
assert isBuildingSet(D, Irr)
assert (set elementaryDivisors(D, 300) === set {3, 4, 25})
G = Irr|{10, 20, 50, 100}
assert isBuildingSet(D, G)
assert(set factors(D, G, 300) === set {3, 100})

///


-- TEST #4
-----------------------------------------------------------------
TEST ///

B = booleanLattice 3
Irr = set irreducibles B
bs = {
    {}, {"011"}, {"110"}, {"101"}, {"111"}, 
    {"011", "111"}, {"110", "111"}, {"101", "111"},
    {"110", "011", "111"}, {"110", "101", "111"}, {"101", "011", "111"},
    {"110", "011", "101", "111"}
    }
assert(set((buildingSets B)/set) === set apply(bs, G -> Irr + set G) )

///

-- TEST #5
-----------------------------------------------------------------
TEST ///

G = graph {{a, b}, {a, c}, {b, c}, {b, d}, {b, e}, {c, d}, {c, e}, {d, e}}
assert isSimpleVertex(G, e)
assert not isSimpleVertex(G, b)
assert isStronglyChordal G
assert not isStronglyChordal sunGraph 4

///
