"""
A simple python program showing how to use regular
expressions to write a paren-matching recursive parser.

This parser recognises items enclosed by parens, brackets,
braces and <> symbols, but is adaptable to any set of
open/close patterns.  This is where the re package greatly
assists in parsing. 

Published by Gene Olson at
https://stackoverflow.com/a/39263467/4063520
"""

import re

# The procedure below matches string s and returns a
# recursive list matching the nesting of the open/close
# patterns in s.

def match_nested(s, term="", matching=None):
    if matching is None:
        # The keys to the dictionary below are the opening strings,
        # and the values are the corresponding closing strings.
        # For example "(" is an opening string and ")" is its
        # closing string.

        matching = { "(" : ")",
                     "[" : "]",
                     "{" : "}",
                     "<" : ">",
                     '"' : '"',
                     "'" : "'",
                     "BEGIN" : "END" }

    rexp = list(matching.keys())
    rexp.extend(list(matching.values()))
    rexp = [re.escape(i) for i in list(set(rexp))]
    rexp = f"( .*? )( {'|'.join(rexp)} | $ )( .* )" 

    # The pattern below recognises a sequence consisting of:
    #    1. Any characters not in the set of open/close strings.
    #    2. One of the open/close strings.
    #    3. The remainder of the string.
    # 
    # There is no reason the opening pattern can't be the
    # same as the closing pattern, so quoted strings can
    # be included.  However quotes are not ignored inside
    # quotes.  More logic is needed for that....

    pat = re.compile(rexp, re.X | re.DOTALL)
    
    def match_nested_impl(s, term):
        lst = []
        while True:
            m = pat.match(s)

            if m.group(1) != "":
                lst.append(m.group(1))

            if m.group(2) == term:
                return lst, m.group(3)

            if m.group(2) in matching:
                item, s = match_nested_impl(m.group(3), matching[m.group(2)])
                lst.append([m.group(2), item, matching[m.group(2)]])
            else:
                raise ValueError("After <<%s %s>> expected %s not %s" %
                                 (lst, s, term, m.group(2)))

    return match_nested_impl(s, term)

# Unit test.

if __name__ == "__main__":
    for s in ("simple string",
              """ "double quote" """,
              """ 'single quote' """,
              "one'two'three'four'five'six'seven",
              "one(two(three(four)five)six)seven",
              "one(two(three)four)five(six(seven)eight)nine",
              "one(two)three[four]five{six}seven<eight>nine",
              "one(two[three{four<five>six}seven]eight)nine",
              "oneBEGINtwo(threeBEGINfourENDfive)sixENDseven",
              "ERROR testing ((( mismatched ))] parens"):
        print("\ninput", s)
        try:
            lst, s = match_nested(s)
            print("output", lst)
        except ValueError as e:
            print(str(e))
    print("done")
