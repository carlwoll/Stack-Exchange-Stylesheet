# Stack-Exchange-Stylesheet
Mathematica stylesheet to support simple copy/paste operations in the Mathematica StackExchange web site

This repository initially contains the following:

  1. A package "CreateStylesheet.m" that creates a stylesheet.
  2. A package "StackExchange.m" that includes functions used by the stylesheet.
  3. A copy of the "Stack Exchange.nb" style sheet.
  
With a Mathemtica notebook using the Stack Exchange style sheet, and a the package "StackExchange.m" on the package loading path, one can create StackExchange cells that will mimic the look of the mathematica.stackexchange.com web site (hereafter referred to as MSE), and the cells can be easily copy/pasted into MSE.

A typical workflow will be as follows:

  1. Create a Text cell using Alt-7
  2. Hit tab to convert the Text cell into a StackExchange cell
  3. Enter MSE markdown text into the cell.
  4. Use right-click, shift-enter or enter an initial tab to convert the markdown into a display version.
  5. Copy the cell or cells (using normal copy, not copy as) and paste content into MSE
