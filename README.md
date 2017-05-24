# CL-Morse - morse support for Common Lisp

CL-Morse adds morse support for Common Lisp. I.e. you can actually
write all (well, almost all) your code in morse. Show this to your
friends and colleagues and say, "Well, can your programming language
do _this_!?" They will totally begin to use Lisp.


## Documentation

Examples can be seen in `example.lisp`.


### Morse representation

Dot/dit and dash/dah are represented by dot and hyphen. Characters are
separated by a single non-dit-dah character and words are separated by
two or more non-dit-dah characters.

When translating to morse for use with code the character delimiter is
by default equal-sign because of the greater readability for symbols
than having all symbols being surrounded by vertical bars. The default
word delimiter is two spaces.

Strings and symbols are of course represented as their respective types.

Numbers and single characters are represented by symbols, numbers
beginning with N and characters beginning with C. Thus:

    (to-morse 1)
    => N....-=..---
    
    (to-morse #\a)
    => C.-


### Reference

#### Run morse code

(Macro) **morse-code** _&body_ body => result*  
Runs morse code, i.e. translates all symbols and strings from _body_
from morse, returning the result.

(Function) **enable-morse-mode** _&optional_ morse-word (end-sym ...-.-) => t  
Sets first character of _morse-word_ as macro character or, if
_morse-word_ starts with #, sets the first and second character as
dispatch macro character. The reader macro will translate the next
sexp like **morse-code**.  
If _morse-word_ is encountered (the rest of _morse-word_ is converted
to upper case and interned; then the macro character can be followed
by this symbol to make up the _morse-word_) sexps will be read until
EOF or _end-sym_ is encountered.  
The default _end-sym_ is the "end of work" morse code.  
The readtable is saved for **disable-morse-mode** at the first call;
thus, multiple macro characters can be set.  
If _morse-word_ is nil it will be treated as if **enable-morse-mode**
was called with two times with "MORSE" and "morse" respectively.

(Function) **disable-morse-mode** => ...  
Sets the readtable to the value before **enable-morse-mode** was
called for the first time.


#### Convert to morse

(Generic function) **to-morse** x _&optional_ (char-delim #\= word-delim-str "  ")
=> object  
Translates _x_ into morse understood by **parse-morse** with
characters delimited by _char-delim_ and words delimited by
_word-delim-str_. All implementations are defined with the specified
defaults. Unimplemented types and objects with unsupported characters
returns _nil_.

(Function) **string->morse** str _&optional_ (word-delim-n 2) => string  
Convenience function. Translates string _str_ to morse with characters
delimited by space and words delimited by _word-delim-n_ spaces.

(Macro) **morsify-with** (char-delim _&optional_ (word-delim-str "  ")) _&body_ body => result*  
Translates _body_ to morse using _char-delim_ as character delimiter
and _word-delim-str_ as word delimiter.  
If _body_ is longer than one sexp multiple values are returned. The
macro is thus intended for copy-pasting from the REPL.

(Macro) **morsify** _&body_ body => result*  
Calls **morsify-with** with the default character and word delimiters.


#### Parse morse

Function **parse-morse** m => object  
Translates _m_ from morse.

Function **morse->string** str => string  
Convenience function. Translates str from morse without type checking.


## CL-Morse testimonies

All these testimonies are totally what these people said.

> If this had been around in the nineties Common Lisp would totally
> have been the duct tape that held the Internet together, except of
> course that it wouldn't be duct tape, just some pleasant sticky
> stuff!  
> _(Andrew Leonard)_

> It would not be far from the truth to say that a hacker about to
> write a program decides what language to use, at least
> subconsciously, based on the total number of different characters
> he'll have to type. I will totally redefine Arc based on morse!  
> _(Paul Graham)_

> I will totally begin to use Lisp!  
> _(My wife, and she's totally a programmer)_

> I will totally begin to let you use Lisp at work!  
> _(My boss)_

> I have always had a feeling that my lifetime was limited by the
> number of unique characters I typed on my keyboard. Being able to
> write all my code almost entirely using dot and dash has totally
> reduced my fear of an early death.  
> _(Søren Kierkegaard)_
