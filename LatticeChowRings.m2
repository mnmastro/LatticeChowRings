-- -*- coding: utf-8 -*-

newPackage(
        "LatticeChowRings",
        Version => "0.2", 
        Date => "November 10, 2022",
        Authors => {
	     {Name => "Matthew Mastroeni", 
		 Email => "mmastro@iastate.edu", 
		 HomePage => "https://mnmastro.github.io/"
		 }
             },
        Headline => "Chow rings of atomic lattices",
	AuxiliaryFiles => true,
        DebuggingMode => true,
	PackageExports => {
	    "SimplicialComplexes",
	    "Graphs",
	    "Posets",
	    "Matroids"
	    }
        )


-- REFERENCES
-----------------------------------------------------------------

-- [BES] = S. Backman, C. Eur, and C. Simpson. 
--         Simplicial generation of Chow rings of matroids. 
--         arXiv:1905.07114 [math.CO]

-- [Ox]  = J. Oxley. Matroid theory. Second edition. 
--         Oxford Graduate Texts in Mathematics, 21. 
--         Oxford University Press, Oxford, 2011.

-- TO DO: 1. Finish documenting all exported/overloaded functions.
--    	  2. Internally document with comments all unexported functions.

-- Record any symbols or functions (except "net") used in each file below.
-- Comment out the name of the function/symbol if it is not exported.

-- Type names must be exported if they are used in the documentation
-- of an overloaded function.

-- ?? = undocumented
-- ** = tested
-- ++ = double checked


-- TO DO:

-- 1. Add hyperplane arrangement example to factors.

export {
    
--ChowRings.m2
    "Augmented",      	      	  -- option for matroidChowRingFiltration ??, totalCoatomOrder ??
    "augmentedBuildingSet",    	  -- documented ++
    "AugmentedVariable",	  -- option for idealAugmentedChowRing ++,  matroidChowRingFiltration ??
    "buildingSets",    	       	  -- ** documented ++
    "factors",	      	      	  -- ** documented ++
    "idealAugmentedChowRing",	  -- ??      
    "idealChowRingFY",    	  -- ??
    "isBuildingSet",    	  -- ** documented ++
    "matroidChowRingFiltration",  -- ??
    "nestedSetComplex",	       	  -- ** documented ++
    --"Presentation",    	  -- overloaded, option for idealAugmentedChowRing, ??
    "RingOptions",    	      	  -- option for idealGradedMoebiusAlgebra, ?? 
    "VariableOrder",	    	  -- option for idealChowRingFY, ??

--GradedMoebius.m2"
    "idealGradedMoebiusAlgebra",  -- documented ++
    "isCChordal",	     	  -- documented ++
    "isSimpleVertex",	     	  -- ** documented ++
    "isStrongElimOrder",    	  -- documented ++
    "isStronglyChordal",    	  -- ** documented ++
    "isStronglyTChordal",    	  -- documented ++
    "strongElimOrder",	      	  -- documented ++
    "sunGraph",	   	      	  -- documented ++

--PosetsAddOns.m2  
    "coatoms",    	       	  -- documented ++
    "completeIrreducibleList",	  -- cache symbol used by elementaryDivisors, irreducibles, ??
    "elementaryDivisors",    	  -- ** documented
    "elementaryDivisorIsomorphism", -- ??
    "irreducibles",    	   	  -- ** documented
    "irreducibleFactors",     	  -- documented
    "irreduciblesGraph",	  -- ??
    "isIrreducible",	    	  -- documented ++
    -- isomorphism    	      	  -- overloaded, ??
    "Output",     	      	  -- option for irreducibleFactors, ??
    "PosetMap",	       	   	  -- exported type name, ??
    "posetMap",	       	   	  -- ??
    "productPoset",    	       	  -- ??
    -- source	     	     	  -- overloaded, ??
    -- target	     	     	  -- overloaded, ??
    "totalCoatomOrder"	       	  -- ??
    }
--exportMutable {}




-------------------------------------------
--- LOAD AUXILIARY FILES ------------------
-------------------------------------------

load "./LatticeChowRings/ChowRings.m2"

load "./LatticeChowRings/GradedMoebius.m2"

load "./LatticeChowRings/PosetsAddOns.m2"



beginDocumentation()

load "./LatticeChowRings/ChowRingsDoc.m2"

load "./LatticeChowRings/GradedMoebiusDoc.m2"

load "./LatticeChowRings/PosetsAddOnsDoc.m2"

undocumented {
    (net, PosetMap),
    }

-- TESTS

load "./LatticeChowRings/LatticeChowRingsTests.m2"




end


-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.


restart
uninstallPackage "LatticeChowRings"
installPackage "LatticeChowRings"
check LatticeChowRings











