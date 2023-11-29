from match_nested import match_nested
import re
import sys

# Check if the file path is provided as a command-line argument
if len(sys.argv) < 2:
    print("Please provide the file path as a command-line argument.")
    sys.exit(1)

file_path = sys.argv[1]

with open(file_path, 'r') as file:
    code = ''.join(file.readlines())

lst, _ = match_nested(code, matching= { "(" : ")" })

def apply_list_recursive(lst, regex, func):
    for index, item in enumerate(lst):
        if type(item) is list:
            apply_list_recursive(item, regex, func)
        else:
            if re.match(regex, item, re.DOTALL):
                func(lst, index)

def comment_equivalence(lst, index):
    lst[index] = lst[index].replace('EQUIVALENCE', '!>>>>EQUIVALENCE')
    index += 1
    while True:
        def comment_linebreaks_recursive(lst):
            for item in lst:
                if type(item) is list:
                    comment_linebreaks_recursive(item)
                else:
                    item = item.replace('\n', '\n!>>>>')
        
        comment_linebreaks_recursive(lst[index])
        if lst[index + 1].strip(' &\n') != ',':
            break
        lst[index + 1] = lst[index + 1].replace('\n', '\n!>>>>')
        index += 2

apply_list_recursive(lst, r'.*\s+EQUIVALENCE\s*', comment_equivalence)

def join_list_recursive(lst):
    s = ''
    for item in lst:
        if type(item) is list:
            s += join_list_recursive(item)
        else:
            s += item
    return s

code = join_list_recursive(lst)
with open(file_path, 'w') as file:
    file.write(code)

