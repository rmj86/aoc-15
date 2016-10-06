# -*- coding: utf-8 -*-
#==============================================================================
# --- Day 4: The Ideal Stocking Stuffer ---
#  
#  Santa needs help mining some AdventCoins (very similar to bitcoins) to use 
#  as gifts for all the economically forward-thinking little girls and boys.
#  
#  To do this, he needs to find MD5 hashes which, in hexadecimal, start with 
#  at least five zeroes. The input to the MD5 hash is some secret key (your 
#  puzzle input, given below) followed by a number in decimal. To mine 
#  AdventCoins, you must find Santa the lowest positive number (no leading 
#  zeroes: 1, 2, 3, ...) that produces such a hash.
#  
#  For example:
#  
#    - If your secret key is abcdef, the answer is 609043, because the MD5 
#      hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it
#      is the lowest such number to do so.
#    - If your secret key is pqrstuv, the lowest number it combines with to 
#      make an MD5 hash starting with five zeroes is 1048970; that is, the 
#      MD5 hash of pqrstuv1048970 looks like 000006136ef....
#  
#  Your puzzle input is yzbqklnj.
#==============================================================================

import hashlib
import time                                                

def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        print '%r (%r, %r) %2.2f sec' % \
              (method.__name__, args, kw, te-ts)
        return result
    return timed

@timeit
def solution_1():
    i = 0
    md5 = hashlib.md5
    while True:
        i += 1
        s = "yzbqklnj"+str(i)
        h = md5(s).hexdigest()
        if h[:5] == '00000':
            break
    return i

#==============================================================================
# --- Part Two ---
# 
# Now find one that starts with six zeroes.
# 
# Your puzzle input is still yzbqklnj.
#==============================================================================

@timeit
def solution_2():
    i = 0
    md5 = hashlib.md5
    while True:
        i += 1
        s = "yzbqklnj" + str(i)
        h = md5(s).hexdigest()
        if h[:6] == '000000':
            break
    return i

#==============================================================================
# main
#==============================================================================

if __name__ == "__main__":
    print solution_1()
    print solution_2()
    