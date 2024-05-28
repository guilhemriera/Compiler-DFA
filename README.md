# Compiler of Deterministic Finite Automata (DFA)
## Introduction
This project is a compiler of Deterministic Finite Automata (DFA) that reads a DFA description that create a c file that implements the DFA. The compiler is written in ocaml and uses ocamllex and ocamlyacc to generate the lexer and parser.

## How to use
To use the compiler you need to have ocaml installed in your machine. To compile the compiler you need to run the following commands:
```bash 
touch .depend
make depend
make
```

After that you can run the compiler with the executable pcfloop. If you run pcfloop without any arguments you can write the DFA description in the terminal and the compiler will generate the c file but you can also pass the DFA description in a file as an argument to the compiler. 

## DFA description
The DFA description must be in the following format:
```bash
automaton {
    letter = [letter1 letter2 ... letterN];
    state = [state1 state2 ... stateN];
    init = InitialState;
    final = [FinalState1 FinalState2 ... FinalStateN];
    transition = [
        (StateA - LetterI -> StateB)
        ...
        (StateX - LetterY -> StateZ)
    ];
} filename
;;
```

in the description above you need to replace the following:
- letter1, letter2, ..., letterN: the letters that the DFA can read; letters are alphabetical string of free size; only separated by space; and without any " or '.
- state1, state2, ..., stateN: the states of the DFA; states are alphabetical string of free size; only separated by space; and without any " or '.
- InitialState: the initial state of the DFA; it must be one of the states defined in the state list.
- FinalState1, FinalState2, ..., FinalStateN: the final states of the DFA; they must be one of the states defined in the state list.
- transition: is list of transitions of the DFA; each transition is a tuple with the following format: (StateA - LetterI -> StateB); where StateA is the state that the transition starts; LetterI is the letter that the transition reads; and StateB is the state that the transition goes to.
- filename: the name of the file that the compiler will generate the c code.

## Example
The following is an example of a DFA description:
```bash
automaton {
    letter = [a sh];
    state = [Qone Qtwo];
    init = Qone;
    final = [Qtwo];
    transition = [
        (Qone - a -> Qtwo)
        (Qtwo - a -> Qone)
        (Qtwo - sh -> Qtwo)
        (Qone - sh -> Qone)
    ];
} example
;;
```

## Simplification of the DFA
transitions of the form (StateA - LetterI -> StateA) are implicit in the description of the DFA but can be also written explicitly.

## Example of a simplified DFA
```bash
automaton {
    letter = [a sh];
    state = [Qone Qtwo];
    init = Qone;
    final = [Qtwo];
    transition = [
        (Qone - a -> Qtwo)
        (Qtwo - a -> Qone)
        (Qtwo - sh -> Qtwo)
        (Qone - sh -> Qone)
    ];
} example
;;

automaton {
    letter = [a sh];
    state = [Qone Qtwo];
    init = Qone;
    final = [Qtwo];
    transition = [
        (Qone - a -> Qtwo)
        (Qtwo - a -> Qone)
    ];
} exampleSimplified
;;
```

## Exemple
Some unadapted semantics code are detected during compilation and are presented in the code_exemple/error_code/
description of DFA for detecting of the parity of the letter a in a word is in the code_exemple/valide_code/automate_N_a_pair.txt 
