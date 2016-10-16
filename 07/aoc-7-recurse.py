bitmask = 2**16-1

def getData():
    lines = []
    with open("input.txt") as f:
        for line in f.readlines(): lines.append(line.rstrip("\n"))
    return lines

def splitLine(line):
    words = line.split()
    key = words[-1]
    eqn = words[:-2]
    return (key, eqn)

def evalKey(key, eqns, values):
    if values.has_key(key): return values[key]
    
    def get(k):
        if k.isdigit(): return int(k)
        else: return evalKey(k, eqns, values)
    def update(v):
        values[key] = bitmask & v
        return values[key]
    
    eq = eqns[key]
    if len(eq) == 1:        return update(  get(eq[0]))  # constant
    elif eq[0] == "NOT":    return update(~ get(eq[1]))
    elif eq[1] == "AND":    return update(get(eq[0]) &  get(eq[2]))
    elif eq[1] == "OR":     return update(get(eq[0]) |  get(eq[2]))
    elif eq[1] == "LSHIFT": return update(get(eq[0]) << get(eq[2]))
    elif eq[1] == "RSHIFT": return update(get(eq[0]) >> get(eq[2]))
    else: print "unparsable equation: {}".format(eq)

if __name__ == "__main__":
    eqns = {key:eqn for key,eqn in map(splitLine, getData())}
    
    vs = {}
    a = evalKey("a", eqns, vs)
    print "part 1: {}".format(a)
    
    del eqns["b"]
    vs = {"b": a}
    a = evalKey("a", eqns, vs)
    print "part 2: {}".format(a)
