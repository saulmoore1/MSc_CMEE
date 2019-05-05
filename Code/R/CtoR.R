#!/usr/bin/env R

dyn.load("../Sandbox/CtoR.so")

tree <- as.character("((A,B),(C,D));")
Newick_R <- function(tree) {
  .Call("count_string_C", tree)
}
Newick_R(tree)

# tree <- paste(as.character(letters[1:26]), collapse = "")