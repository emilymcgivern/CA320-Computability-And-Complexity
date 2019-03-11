-- Design a pushdown automaton ijk to accept the language {aibjck | i,j,k ≥ 0 and i=j or i=k}. 
-- This pushdown automaton should accept the following strings:
 
-- ""
-- "b"
-- "abbc"
-- "aabbc"
-- "aabbcc"
 
-- And reject the following strings:
 
-- "a"
-- "aabc"
-- "abbcc"

ijk = (0, [3,6], [((0, "", ""), (1, "$")),
				  ((1, "a", ""), (1, "a")),
				  ((1, "", ""), (2, "")),
				  ((2, "b", "a"), (2, "")),
				  ((2, "", "$"), (3, "")),
				  ((3, "c", ""), (3, "")),
				  ((1, "", ""), (4, "")),
				  ((4, "b", ""), (4, "")),
				  ((4, "", ""), (5, "")),
				  ((5, "c", "a"), (5, "")),
				  ((5, "", "$"), (6, ""))

	])
