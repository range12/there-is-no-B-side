
# There-is-no-B-side

<i>
One tape to hold it all, One head to seek along, <br/>
One set of symbols to emulate them all and in their many states drive them. <br/>
</i>
<br/>
Alan Turing, <i>The Lord of the Tape</i>

## Description

### A Haskell take on a single-tape//single-head Turing machine

Symbols, states and transitions are formally specified in JSON.  
JSON machine specs are decoded and run by the VM/Main executable.

### Plans within plans

WIP helper programs aim to automate the massive tasks of generating states, transitions and symbols for advanced machines.  

## Guest machine translator (WIP)

A nesting pre-processor is formally described as a template skeleton YAML host machine spec.  
The goal is to lift an existing JSON spec with arbitrary (yet correct) alphabet,  
transitions, states to a whole new machine spec.  
The result shall be a spec of nested machines recursively emulating their guest.
