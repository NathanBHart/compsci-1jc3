## **Derivative Solver** - Assignment #4 COMPSCI 1JC3
### Nathan Hart - Nov 28, 2022
---
**Objective:** Generate a library of basic functions for working with a custom mathematical expression datatype. Include the ability to evaluate an expression at a given *x* value, the ability to symbolically differentiate a given math expression, the ability to simplify an expression (limited), and the ability to produce a string representation for a given math expression.

**Learning Objectives:**
- Learn how to use type classes and instances in Haskell
- Improve competency in using sum algebraic datatypes
- Implement increasingly complex behaviors (e.g. symbolic simplification) using recursion and pattern matching.
- Implement quickcheck tests to quickly test the behavior of Haskell code.
- Write a thought out testing plan to manually check edge cases and potential weaknesses.
- Understand and manage complex numerical behavoirs and datatypes.

**Possible Extensions:**
- Add support for more functions and constants to the MathExpr datatype
- Improve simplification function to be able to properly identify commutivity in addition and multiplication.
- Possibly extent simplication function to be able to identify more areas of simplification.
- Allow pretty function to behave differently for (Num a) => MathExpr a than for MathExpr of other types, specifically simplify the MathExpr before creating a string representation.
