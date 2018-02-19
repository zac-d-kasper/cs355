import re
import ast
import decimal

#Zac Kasper - 011468596
#CptS 355 - HW5

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
def dictPush(i, d):
    dictstack.append((i, d))

#modify to append on tuple at top of dictstack
def define(name, value):
    dictstack[-1][1].update({name: value})
    #adds value to top dictstack tuple

def dynamicLookup(name):
    for i in range((len(dictstack) - 1), -1, -1):
        if name in dictstack[i][1]:
            opPush(dictstack[i][1].get(name))
            break
    #looks from top of dict stack for variable lookups

def staticLookup(name, scope):
    for i in range(scope, -1, -1):
        if name in dictstack[i][1]:
            #searches dictionary in dictstack tuple
            opPush(dictstack[i][1].get(name))
            return i #returns index of dict that "name" is located

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
def stack():
    for item in reversed(opstack):
        print(item)

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
        if item == '}':
            return curr
        elif item == '{':
            curr.append(groupMatcher(it))
        else:
            temp = typeconvert(item)
            curr.append(temp)
    return False

def parse(L):
    res = []
    it = iter(L)
    for item in it:
        if item == '{':
            res.append(groupMatcher(it))
        elif item == '}':
            return False
        else:
            temp = typeconvert(item)
            res.append(temp)
    return res

def typeconvert(item):
    if item[0] == '/':
        return item
    elif item[0] == '[':
        temp = item
        temp = temp.replace(' ', ',')
        return ast.literal_eval(temp)
    elif item[0].isalpha():
        return item
    else:
        for c in item:
            if c == '.':
                return decimal(item)
            if c == '-':
                return int(item)
                #implies item is a negative integer
            if not isinstance(int(c), int):
                return item
        return int(item)


def interpret(codearr, mode, scope):
    #"scope" holds dictionary index for current function
        #not used in dynamic mode
    dictPush(len(dictstack), {})
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
        elif item == 'pop': opPop()
        elif item == 'for':
            code = opPop() #code array
            final = opPop()
            incr = opPop()
            init = opPop()
            current = init
            while current <= final:
                opPush(current)
                interpret(code, mode, scope)
                current += incr
        elif item == 'forall':
            code = opPop()
            arr = opPop()
            for item in arr:
                opPush(item)
                interpret(code, mode, scope)
        elif isinstance(item, list):
            opPush(item)
        elif isinstance(item, (int, float)):
            opPush(item)
        elif item[0] != '/': #variable
            if mode == "dynamic":
                dynamicLookup(item)
            elif mode == "static":
                staticLookTemp = staticLookup(item, scope)
                #modifies scope to be from where last function was called
                    #any variables will be searched after a function call
                    #therefore any relevant variables will be at this scope
            if isinstance(opstack[-1], list): #tests if code
                code = opPop()
                if mode == "static":
                    if staticLookTemp != scope:
                        interpret(code, mode, staticLookTemp)
                    else:
                        interpret(code, mode, len(dictstack))
                elif mode == "dynamic":
                    interpret(code, mode, len(dictstack)-1)
        else:
            opPush(item)

def dictStackPrint():
    for i in range(len(dictstack)-1, -1, -1):
        print("----%d----" % dictstack[i][0]) #dictionary index
        for key in dictstack[i][1]:
            print("%s    %s" % (key, dictstack[i][1][key]))

def stackprinter():
    print("=====OpStack=====")
    stack()
    print("====DictStack====")
    dictStackPrint()
    print("=======End=======")

def interpreter(s, mode):
    interpret(parse(tokenize(s)), mode, 0)
    stackprinter()

interpreter("/x 4 def /g { x stack } def /f { /x 7 def g } def f", "dynamic")
clear()
print("\n\n")
interpreter("/m [25 50] 1 get def /n [100 1] 0 get def /egg1 {/m 25 def n} def /chic { /n 1 def /egg2 { n } def m n egg1 egg2 stack } def n chic", "dynamic")