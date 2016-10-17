from collections import defaultdict

bitmask = 2**16-1

fs = { "ID"    : lambda a:    bitmask &   a
     , "NOT"   : lambda a:    bitmask & ~ a
     , "AND"   : lambda a, b: bitmask & a & b
     , "OR"    : lambda a, b: bitmask & a | b
     , "LSHIFT": lambda a, b: bitmask & a << b
     , "RSHIFT": lambda a, b: bitmask & a >> b
     }
 
def parseLine(line):
    (eqn, var) = line.split("->")
    var = var.strip()
    t = eqn.split()
    if len(t) == 1:   return (var, ["ID", t[0]])
    elif len(t) == 2: return (var, t)
    else:             return (var, [t[1], t[0], t[2]])
    
def makeDepMap(eqns):
    dependency = defaultdict(lambda: [])
    dependents = defaultdict(lambda: [])
    for (var, expr) in eqns.items():
        for d in expr[1:]:
            if not d.isdigit():
                dependency[var] . append(d)
                dependents[d] . append(var)
    return (dependency, dependents)

def reduce(eqns):
    values = {}
    (dependency, dependents) = makeDepMap(eqns)
    queue = [var for var in eqns if dependency[var]==[]]
    def get(s):
        if s.isdigit(): return int(s)
        else:           return values[s]
    def evalEqn(f_name, *terms):
        return fs[f_name](*map(get, terms))
        
    for var in queue:
        values[var] = evalEqn(*eqns[var])
        for d in dependents[var]:
            dependency[d].remove(var)
            if dependency[d] == []:
                queue.append(d)
    return values

if __name__ == "__main__":
    eqns = {}
    with open("input.txt") as f:
        for line in f.readlines():
            (var, eqn) = parseLine(line)
            eqns[var] = eqn
    # Part 1
    a = reduce(eqns)["a"]
    print "part 1: {}".format(a)
    # Part 2
    eqns["b"] = ["ID", str(a)]
    a = reduce(eqns)["a"]
    print "part 1: {}".format(a)
