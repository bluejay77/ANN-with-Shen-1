\\ AJY 2015-12-13
\\
\\ From George F Luger: ARTIFICIAL INTELLIGENCE, 4th Edition
\\ Pearson--Addison-Wesley, ISBN 0-201-64866-0
\\
\\ Exercise 1. from 10.8 Page 468
\\
\\ 1. Make a McCulloch-Pitts neuron that can calculate the logic
\\ function implies, =>
\\
\\
\\ A => B is equivalent with ~A V B
\\ Make the neuron from the OR (V) McCulloch-Pitts neuron
\\
\\ With a McCulloch-Pitts neuron, ~X is -X + 1, easily.
\\ Then X => Y == ~X V Y == (-X + 1) + Y - 1 == -X + Y.
\\
\\ First load Ramil Farkshatov's defstruct package:
\\
\\ (load "defstruct.shen")
\\
\\ After that, load and run this file:
\\
\\ (load "mcc-p.shen")
\\ (ann-demo)
\\
\\
\\ The model to use RF's defstruct:
\\ 
\\  (defstruct person
\\    (name string)
\\    (last-name string)
\\    (gender symbol)
\\    (age number))



(defstruct neuron1
  (nr-inputs number)
  (inputs-vec vector)
  (weights-vec vector)
  (activation-level number)
  (threshold-function (number --> number))
  (neuron-output number))



\\ Calling (transfer-function N) where N is a neuron, is equivalent
\\ with firing the neuron N, ie. computing its output

(define transfer-function
  { neuron1 --> number }
  N ->
    (let
      AL (activation-level-aux N)
      _  (neuron1-activation-level-> N AL)
      FF (neuron1-threshold-function N)
      _  (neuron1-neuron-output-> N (FF (neuron1-activation-level N)))
      (neuron1-neuron-output N)))



(define activation-level-aux
  { neuron1 --> number }
  N -> (activation-level-aux-h N 1 0))



(define activation-level-aux-h
  { neuron1 --> number --> number --> number }
  N Counter Sum ->
    Sum where (> Counter (neuron1-nr-inputs N))
  N Counter Sum ->
    (activation-level-aux-h
      N (+ 1 Counter)
        (+ Sum (* (<-vector (neuron1-inputs-vec N) Counter)
                  (<-vector (neuron1-weights-vec N) Counter)))))


\\ The McCulloch-Pitts threshold function

(define tres-f
  { number --> number }
  X -> 1 where (>= X 0)
  X -> -1)



\\ Create the McCulloch-Pitts neuron in question:

(set implication-neuron
  (mk-neuron1
    3          \\ Three inputs
    (vector 3) \\ Inputs vector
    (vector 3) \\ Weights vector
    0          \\ Dummy for activation level
    (function tres-f) \\ The McCulloch-Pitts threshold function
    0          \\ Dummy for neuron output number
  ))


\\ Set the input bias and the input weights


(neuron1-inputs-vec-> (value implication-neuron) (@v 0 0 1 <>)) \\ X,Y,bias

(neuron1-weights-vec-> (value implication-neuron) (@v -1 1 0 <>)) \\ ~X V Y


\\ And the "main function":

(define ann-demo
  { --> (list A) }
  ->
  (let
    _ (output "~%Give A: ")
    A (input)
    VecA (neuron1-inputs-vec (value implication-neuron))
    _ (vector-> VecA 1 A)
    _ (neuron1-inputs-vec-> (value implication-neuron) VecA)
    _ (output "~%Give B: ")
    B (input)
    VecB (neuron1-inputs-vec (value implication-neuron))
    _ (vector-> VecB 2 B)
    _ (neuron1-inputs-vec-> (value implication-neuron) VecB)
    Out (transfer-function (value implication-neuron)) \\ Fire neuron
    _ (output "~%Output of neuron: ~A~%" Out)
    []))



\\ If one will try the (ann-demo), it will give the following
\\ behaviour:
\\
\\ A B (ann-demo)
\\ 1 1 1
\\ 1 0 -1
\\ 0 1 1
\\ 0 0 1
\\
\\ which indeed is the truth table for the mathematical
\\ material implication.
\\
