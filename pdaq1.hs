--declaring new types
-- defining pda, result, transition and configuration as these are not defined by haskell already
--((current state, next input, what is at top of stack), (new state, what is to be pushed))
type Transition = ((Integer,String,String),(Integer,String))
-- (start state, [end state], [Transitions])
type PDA = (Integer,[Integer],[Transition])
--only 2 possible results are Accept and Reject
data Result = Accept | Reject deriving Show
-- (current state, remaining input, contents of the stack)
type Configuration = (Integer,String,String)

run :: PDA -> String -> Result
--start is start state
--finish is end state
--transition is transitions of pushdown automaton

--input is string
-- "" is empty string

--outputs a result - Accept or Reject
run (start, finish, transition) "" = Accept
run (start, finish, transition) input = run' (start, finish, transition) [(start, input, "")]

run' :: PDA -> [Configuration] -> Result
run' (start, final, transition) (head:input) = 
  if accept head final == True 
    then Accept
  else run' (start, final, transition) (input ++ (steps head transition []))


run' (start, final, transition) [] = Reject

--check if a list of configurations includes at least one state that means we can accept the string
-- start is start state
-- input is string
 
accept :: Configuration -> [Integer] -> Bool
-- check if a list of configurations includes at least one state that means we can accept the string
-- if there is no state return false
accept (start, input, part) (headf:final) =
  if input == "" && start == headf && part == ""  
    then True
  else accept (start, input, part) final


accept (start, input, part) [] = False


step :: Configuration -> Transition -> [Configuration]
-- will apply a transition to a configuration
-- created step functions using truth table in notes 
-- empty list output if the transition cannot be applied
-- 16 steps in total to reflect the 16 in the truth table

--1       0       0   0     0                        None None
step (a,b,"") ((d,"",""),(g,""))
  |a == d = [(g,b,"")]
  |otherwise = []

--2        0       0  0      1                       None Push
step (a,b,"") ((d,"",""),(g,[h]))
  |a==d = [(g,b,[h])]
  |otherwise = []

-- 3              0        0.   1        0           None N/A
step (a, (b:bs), "") ((d, "", [f]), (g, "")) = []

-- 4              0        0.   1         1.         None N/A
step (a, (b:bs), "") ((d, "", [f]), (g, [h])) = []

--5            0        1  0      0                  Right None
step (a,(b:bs),"") ((d,[e],""),(g,""))
  |a==d && b==e = [(g,bs,"")]
  |otherwise = []

--6             0       1  0       1                 Right Push
step (a,(b:bs),"") ((d,[e],""),(g,[h]))
  |a==d && b==e = [(g,bs,[h])]
  |otherwise = []

-- 7             0         1.   1        0.          Right N/A
step (a, (b:bs), "") ((d, [e], [f]), (g, "")) = []

-- 8              0        1 .   1         1         Right N/A
step (a, (b:bs), "") ((d, [e], [f]), (g, [h])) = []

--9       1      0   0     0                         None None
step (a,b,c) ((d,"",""),(g,""))
  |a==d = [(g,b,c)]
  |otherwise = []

--10      1      0  0       1                        None Push
step (a,b,c) ((d,"",""),(g,[h]))
  |a==d = [(g,b,(h:c))]
  |otherwise = []

--11         1        0   1       0                  None Pop
step (a,b,(c:cs)) ((d,"",[f]),(g,""))
  |a==d && c==f = [(g,b,cs)]
  |otherwise = []

--12        1         0   1       1                  None Replace
step (a,b,(c:cs)) ((d,"",[f]),(g,[h]))
  |a==d && c==f = [(g,b,(h:cs))]
  |otherwise = []

--13           1       1  0      0                   Right None
step (a,(b:bs),c) ((d,[e],""),(g,""))
  |a==d && b==e = [(g,bs,c)]
  |otherwise = []

--14           1       1  0       1                  Right Push #pushing h onto whole stack 
step (a,(b:bs),c) ((d,[e],""),(g,[h]))
  |a==d && b==e = [(g,bs,h:c)]
  |otherwise = []

--15             1          1   1       0            Right Pop
step (a,(b:bs),(c:cs)) ((d,[e],[f]),(g,""))
    | a==d && b==e && c==f = [(g,bs,cs)]
    | otherwise = []

--16             1          1   1          1         Right Replace
step (a,(b:bs),(c:cs)) ((d,[e],[f]), (g,[h]))
    | a==d && b==e && c==f = [(g,bs,(h:cs))]
    | otherwise = []

--configuration
--list of transitions
--list of configuratiuons
--output list of next step configurations

steps :: Configuration -> [Transition] -> [Configuration] -> [Configuration]
-- output the list of next step configurations
steps (start, input, part) [] configuration = configuration
steps (start, "", part) transition configuration = configuration
steps (start, input, part) (headt:transition) configuration = steps (start, input, part) transition (configuration ++ (step (start, input, part) headt))


-- all testing --
-- -- pushdown automaton which accepts the language {w ∈ {a,b}*|w=wR}
-- pal = (1,[2],[((1,"a",""),(1,"a")),
--               ((1,"b",""),(1,"b")),
--               ((1,"a",""),(2,"")),
--               ((1,"b",""),(2,"")),
--               ((1,"",""),(2,"")),
--               ((2,"a","a"),(2,"")),
--               ((2,"b","b"),(2,""))])

-- --running some test cases
-- test :: Result
-- test = run pal "abba"




