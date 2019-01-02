import numpy as np
import numpy.random as rnd

def my_gen( list, a, b ):
    n = len(list)
    s0 = 0
    sf = n - a - b
    while True:
        i = rnd.randint(s0,sf)
        part1 = list[i:i+a]
        part2 = list[i+a:i+a+b]
        yield ( part1, part2 )


rnd.seed(1)

rawdata = 'abcdefghijklmnopqrstuvwxyz0123456789'
init = []
for x in rawdata: init.append(x)

print(init)
train = my_gen( init, 7, 3 )

for i in range(10):
    print( next(train) )



