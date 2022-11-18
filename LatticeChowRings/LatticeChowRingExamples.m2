
/// EXAMPLE

M = matroid completeGraph 4
PL = latticeOfFlats M
Irr = irreducibles PL
F = matroidChowRingFiltration(M, Irr)
time isKoszulFiltration F
-- Using the same recursive coatom order to define an analogous filtration
-- on the Chow ring of a matroid with minimal building set
-- seems to also give a Koszul filtration!
///

/// EXAMPLE

M = matroid completeGraph 5
PL = latticeOfFlats M
irreduciblesGraph PL
Irr = irreducibles PL
isBuildingSet(PL, Irr)
idealChowRingFY(PL, Irr)

L = dual PL
L0 = L_* - set {first minimalElements L}
IrrL = irreducibles L
L0 == IrrL
isBuildingSet(L, L0)
idealChowRingFY(L, IrrL)
///


/// EXAMPLE
L = poset {{z,a},{z,b},{z,c},{z,d},{a,e},{b,e},{c,f},{d,f},{e,t},{f,t}}
L0 = L_* - set {first minimalElements L}
isBuildingSet(L, L0)
I = idealChowRingFY(L, L0)
///

/// EXAMPLE

B = booleanLattice 3
isIrreducible B
Irr = irreducibles B
isBuildingSet(B, Irr)
elementaryDivisors(B, "011")

-- Note that the Boolean lattice on a set X is always reducible if
-- X has at least 2 elements. We have B_X = B_X' x B_x if X = X' + {x}.
-- The interval [0, X] in a Boolean lattice is isomorphic to the 
-- Boolean lattice B_X.
-- Hence, X is irreducible in a Boolean lattice if and only if it is an atom.
-- In that case, the Chow ring w.r.t. the irreducibles is just the ground field,
-- so it is always Koszul.  
idealChowRingFY(B, Irr)
idealChowRingFY(B, G)

G ={"001", "010", "100", "111"}
-- This is the ONLY building set that produces a non-quadratic/not Koszul
-- Chow ring.
isBuildingSet(B, G)
I = idealChowRingFY(B, G)


G ={"0001", "0010", "0100", "1000", "1111"}
idealChowRingFY(B, G)
///

/// EXAMPLE

G = graph {{0, 1}, {1, 2}, {0, 2}, {2, 3}, {3, 4}}
M = matroid G
L = latticeOfFlats M
L = dual L
isGeometric L

IrrL = irreducibles L
isBuildingSet(L, IrrL)  
idealChowRingFY(L, IrrL)


V = specificMatroid "vamos"
L = dual latticeOfFlats V
isGeometric L
isAtomic L
IrrL = irreducibles L
///

/// EXAMPLE

G = cycleGraph 4
M = matroid G
L = latticeOfFlats M
IrrL = irreducibles L
isBuildingSet(L, IrrL)  
idealChowRingFY(L, IrrL)

///
