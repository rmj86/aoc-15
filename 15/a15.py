def process_data():
    with open('input.txt') as f:
        data = f.read().splitlines()
    ingredients = []
    for line in data:
        l = line.replace(',', '')
        l = l.split(' ')
        (cap, dur, fla, tex, cal) = [int(l[i]) for i in [2,4,6,8,10]]
        ingredients.append([cap,dur,fla,tex,cal])
    ingredients.sort(key = lambda a: -a[-1])  # sort by decreasing calories
    return ingredients
    
ingredients = process_data()
def product(xs): return reduce(lambda a,b:a*b, xs, 1)

def score(props):
    total = (0,0,0,0)
    for count, ing in zip(props, ingredients):
        total = [t + count*prop for t, prop in zip(total,ing[:-1])]
    return product([max(0,p) for p in total])

# recursively generate all combinations with `count` items, a
# total of `cals` calories, with the given ingredients `ings`.
def combinations(count, cals, ings):
    if len(ings) == 1:
        if cals == count*ings[0][-1]:
            yield [count]
    else:
        ing, ings_ = ings[0], ings[1:]
        for n in range(cals/ing[-1]):
            for comb in combinations(count-n, cals-ing[-1]*n, ings_):
                yield [n]+comb

if __name__ == '__main__':
    print max(map(score, combinations(100, 500,ingredients)))
