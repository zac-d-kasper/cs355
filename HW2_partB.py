import re
import ast
import decimal

#Zac Kasper - 011468596
#CptS 355 - HW2 Part B

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Part A code here (modified/corrected from before)

#"Hot End" will be the last item, AKA index -1
#Operand Stack:
opstack = []

def opPop():
    return opstack.pop(-1)
def opPush(value):
    opstack.append(value)

#Dictionary Stack:
dictstack = []

def dictPop():
    return dictstack.pop(-1)
def dictPush(d):
    dictstack.append(d)
def define(name, value):
    dictstack.append({name: value})

def lookup(name):
    for dictionary in dictstack:
        if name in dictionary:
            opPush(dictionary.get(name))
    #return value associated with name

#Arithmetic operators
    #make sure to check opstack has correct number of
    #parameters and that the types of parameters are correct
        #add, sub, mul, div, mod
def add():
    if len(opstack) >= 2:
        if (type(opstack[-1]) is int or float) and (type(opstack[-2]) is int or float):
            temp2 = opPop()
            temp1 = opPop()
            opPush(temp1 + temp2)
def sub():
    if len(opstack) >= 2:
        if (type(opstack[-1]) is int or float) and (type(opstack[-2]) is int or float):
            temp2 = opPop()
            temp1 = opPop()
            opPush(temp1 - temp2)
def mul():
    if len(opstack) >= 2:
        if (type(opstack[-1]) is int or float) and (type(opstack[-2]) is int or float):
            temp2 = opPop()
            temp1 = opPop()
            opPush(temp1 * temp2)
def div():
    if len(opstack) >= 2:
        if (type(opstack[-1]) is int or float) and (type(opstack[-2]) is int or float):
            temp2 = opPop()
            temp1 = opPop()
            opPush(temp1 / temp2)
def mod():
    if len(opstack) >= 2:
        if (type(opstack[-1]) is int or float) and (type(opstack[-2]) is int or float):
            temp2 = opPop()
            temp1 = opPop()
            opPush(temp1 % temp2)

#Array operators: length, get
def length():
    tempArr = opPop()
    opPush(len(tempArr))
    #assumes top element is an array

def get():
    index = opPop()
    array = opPop()
    target = array[index]
    opPush(target)

#Stack manipulation, print operators:
    #dup, exch, pop, roll, copy, clear, stack
def dup():
    tempNew = opstack[-1]
    opPush(tempNew)
def exch():
    second = opstack[-2]
    opstack[-2] = opstack[-1]
    opstack[-1] = second
def roll():
    rollnum = opPop()
    rangenum = opPop()
    temp = opstack[-rangenum:]
    rolledtemp = temp[rollnum:] + temp[:rollnum]
    del opstack[-rangenum:]
    opstack.extend(rolledtemp)
def copy():
    numItems = opPop()
    templist = opstack[-numItems:]
    opstack.extend(templist)
def clear():
    del opstack[:]
    #deletes all values on the opstack
def stack():
    for item in reversed(opstack):
        print(item)
#def pop(): already defined for opstack?


#Dictionary manipulation operators:
    #dict, begin, end, psDef (def operator is reserved for Python)
    #psDef will pop the value and name from the stack,
        #and call the "define" operator with those values as
        #parameters
def psdict():
    newsize = opPop()
    #takes new size of empty dictionary from opstack
    newDict = dict.fromkeys((range(newsize)))
    opPush(newDict)
def begin():
    topDict = opPop()
    dictstack.append(topDict)
def end():
    dictstack.pop(-1)
def psDef():
    value = opPop()
    tempname = opPop()
    name = tempname.replace("/", "")
    define(name, value)

#PART A ENDS HERE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def tokenize(s):
   retValue = re.findall("/?[a-zA-Z][a-zA-Z0-9_]*|[[][a-zA-Z0-9_\s!][a-zA-Z0-9_\s!]*[]]|[-]?[0-9]+|[}{]+|%.*|[^ \t\n]", s)
   return retValue

def groupMatcher(it):
    curr = []
    for item in it:
        #temp = ast.literal_eval(item)
        if item == '}':
            return curr
        elif item == '{':
            curr.append(groupMatcher(it))
        else:
            temp = typeconvert(item)
            curr.append(temp)
    return False
        #returns False if there's too many open brackets

def parse(L):
    res = []
    it = iter(L)
    for item in it:
        if item == '{':
            res.append(groupMatcher(it))
        elif item == '}':
            return False
            #implies that there's an incorrect amount/placement of
            #closing brackets
        else:
            #evaluate item as a literal (convert '0' -> 0, etc.)
            temp = typeconvert(item)
            res.append(temp)
    return res

# print(ast.literal_eval('0'))

def typeconvert(item):
    if item[0] == '/':
        #any item in the form '/word'
        return item
    elif item[0] == '[':
        #any array with items inside
        temp = item
        temp = temp.replace(' ', ',')
        return ast.literal_eval(temp)
    elif item[0].isalpha():
        #any PS operator
        return item
    else: #covers any integers/decimal values
        for c in item:
            if c == '.':
                return decimal(item)
                #presence of decimal -> return decimal value
            if c == '-':
                return int(item)
                #implies item is a negative integer
            if not isinstance(int(c), int):
                return item
                #implies that item is an operator (not an integer)
        return int(item)

#print(parse(['/thing', '0', '{', '0', 'dict', '{', '1', '}', '}', '0', '[1 2 3]']))
#desired result [0, [0, dict, [1]], 0, [1, 2, 3]]

def interpret(codearr):
#lookup, add, sub, mul, div, mod, length,
# get, dup, exch, roll, copy, clear, stack, dict,
# begin, end, def, for, forall
    for item in codearr:
        if item == 'def': psDef()
        elif item == 'add': add()
        elif item == 'sub': sub()
        elif item == 'mul': mul()
        elif item == 'div': div()
        elif item == 'mod': mod()
        elif item == 'length': length()
        elif item == 'get': get()
        elif item == 'dup': dup()
        elif item == 'exch': exch()
        elif item == 'roll': roll()
        elif item == 'copy': copy()
        elif item == 'clear': clear()
        elif item == 'stack': stack()
        elif item == 'dict': psdict()
        elif item == 'begin': begin()
        elif item == 'end': end()
        elif item == 'pop': opPop()
        elif item == 'for':
            code = opPop() #code array
            final = opPop()
            incr = opPop()
            init = opPop()
            current = init
            while current <= final:
                opPush(current)
                interpret(code)
                current += incr
        elif item == 'forall':
            code = opPop()
            arr = opPop()
            for item in arr:
                opPush(item)
                interpret(code)
        elif isinstance(item, list): #pushes list item
            opPush(item)
        elif isinstance(item, (int, float)):
            opPush(item)
        elif item[0] != '/': #variable
            lookup(item)
            if isinstance(opstack[-1], list): #tests if code
                code = opPop()
                interpret(code)
        else:
            opPush(item)

def interpreter(s):
    interpret(parse(tokenize(s)))

interpreter("/fact {0 dict begin /n exch def 1 n -1 1 {mul} for end} def [1 2 3 4 5] dup 4 get pop length fact stack")
