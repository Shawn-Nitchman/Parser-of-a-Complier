Hello welcome to my pasrser. There should be tons of comments on what my code does. I wanted
too make my code modular however I ran out of time as I have an hour to submit and don't have time to deal with 
that. However the top half of my code is the scanner and the bottom half is the parser. There are a few lines of 
comments that show where the scanner ends and the parser begins. 

*to run it*
put what file you want to scan into the ./code/Input file. Once your program is there you can open parser.rkt
and run the program. If you want to scan the file you can type in (scan "input.txt") you do not need to put the
file path just the name as my code looks into that folder. Also make sure your input file is in a string form.
The parser is the same way with (parse "input01.txt").

*what the scanner does*
So it will open a file read the character one at a time and deterimine if it is valid then assign it the correct
token such as + as add_op or 1234324353 as number. It will do this until it gets to $$ where it will stop reading
and return a list of the whole file scanned. If you just call the (scan "input.txt") it will print out the list or
otherwise give an error with a small amount of help what happened. 

*What the parser does*
So it will call the scanner and once it has the list it will follow the predict rules to create the parse file
I had a lot of toruble with the parser as at first I was thinking in a Von Neumann language where I could have
a list and edit as needed. However that is a side effect and racket is not made for that. You will see that in
my sudo code (./resources/parser sudo code) that with the advance function would manipulate a a list. I figured
out eventually that I had to just call the functions I need in a reverse order of production rule and if needed 
return the rest of the list. That why at the bottom there are a lot of funtions that do a double check and return
the rest of the list. My code does feel redudent but with this simple language that is how I got it to work. 
 