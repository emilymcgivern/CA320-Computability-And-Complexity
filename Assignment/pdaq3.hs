-- Design a pushdown automaton abc to accept the language {w ∈ {a,b,c}* | the number of c's is equal to the number of a's plus the number of b's}. 
-- This pushdown automaton should accept the following strings:
 
-- ""
-- "cabc"
-- "cacbca"
-- "aabbcccc"
-- "acbcbcac"
 
-- And reject the following strings:
 
-- "a"
-- "abc"
-- "cacbcac"

-- check (a+b) against c 
-- order of the a's, b's and c's doesn't matter

abc = (0, [4], [((0, "", ""), (1, "$")),

	            ((1, "a", ""), (1, "A")), 
	            ((1, "a", "AA"), (1, "")), 
	            ((1, "", ""), (2, "")), 

	            ((2, "b", ""), (2, "A")), 
	            ((2, "b", "AA"), (2, "")), 
	            ((2, "", ""), (3, "")),

	            ((3, "c", "A"), (3, "")), 
	            ((3, "c", ""), (3, "AA")),
	            ((3, "a", ""), (1, "A")), 
	            ((3, "a", "AA"), (1, "")),
	            ((3, "b", ""), (2, "A")),
	            ((3, "b", "AA"), (2, "")),
	            ((3, "", "$"), (4, ""))         ])









