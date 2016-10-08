import numpy as np
import re

def mapRegion(f, (x0,y0,x1,y1), a):
    (x1, y1) = (x1+1, y1+1)
    a[x0:x1, y0:y1] = f( a[x0:x1, y0:y1] )

def solution_1(commands):
    f = { "turn off" : lambda b: 0
        , "turn on"  : lambda b: 1
        , "toggle"   : lambda b: 1-b }
    lights = np.zeros((1000,1000), dtype=int)
    for (cmd, bbox) in commands:
        mapRegion(f[cmd], bbox, lights)
    return lights.sum()

def solution_2(commands):
    def nonNegative(a):
        a[a<0] = 0
        return a
    f = { "turn off" : lambda a: nonNegative(a-1)
        , "turn on"  : lambda a: a+1
        , "toggle"   : lambda a: a+2 }
    lights = np.zeros((1000,1000), dtype=int)
    for (cmd, bbox) in commands:
        mapRegion(f[cmd], bbox, lights)
    return lights.sum()

def getCommands():
    with open("input.txt") as f:
        lines = f.readlines()
    pattern = '(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)'
    commands = []
    for line in lines:
        m = re.search(pattern, line)
        c = m.group(1)
        bbox = [int(n) for n in m.group(2, 3, 4, 5)]
        commands.append((c, bbox))
    return commands

if __name__ == "__main__":
    commands = getCommands()
    print solution_1(commands)
    print solution_2(commands)
