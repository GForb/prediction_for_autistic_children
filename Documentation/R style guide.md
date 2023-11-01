# Gordon’s R Instructions

## wherever possible use tidyvere functions https://www.tidyverse.org/

## use the pipe

## Follow the tidyverse style guide https://style.tidyverse.org/, in particular:
  - Meaningful, concise names
  - Spacing and indenting

There is a lot in there about complex features of R - ignore

##Aim to break your code into many functions
  - Each function should do one (maybe two) things
  - The function name, a verb, should reflect what it does
  - If you find a function is doing many things, break it up

## Organising scripts
  - Each script should do one type of task
  - In particular, separate scripts that analyse data from those that organise data
  - It may be useful to put all the functions that complete a particular task in their own script and have a script that defines the functions and then a script that applies then to the data
  - For scripts of functions, work backwards
    - The first function is the one that is needed for the task, it may call a number of sub functions 
    - Next define the sub functions, these in turn may call more sub functions
    - Etc...
    - If a function has many subfunctions, give it its own script 
    - Bundle simpler functions needed into one script.
## Use comments sparingly
  - It should be clear from your code what it is doing without comments
  - By using many functions, with meaningful names, and well organised scripts it should not be necessary to use comments to explain what your code is doing.
  - Comments may explain why or highlight something unusual or surprising

## Use the here package for file paths.
  - Use package::function(), it makes your code a bit more verbose but avoids conflicts and makes it clear where functions are from
  - Exceptions are commonly used functions in particular the tidyverse.

## Use the here package for file paths.
  - Use here::here() for file paths. These paths will be relative to the project directory.

## Finally – do what works, and feels natural
  - Follow the above guidelines loosely
  - Good code is working code




