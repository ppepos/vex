#!/usr/bin/env python

import sys
fname = sys.argv[1]

f = open(fname).read().split('\n')
i = 0
while i < len(f):
    if 'case' in f[i] and '...' in f[i]:
        padding = f[i][:f[i].index('case')]
        suffix = f[i][f[i].index(':')+1:]
        p1, p2 = f[i].split('...')
        p1 = p1.split()[-1]
        p2 = p2.split()[0].strip(':')
        p1 = int(p1, 0)
        p2 = int(p2, 0)
        f.pop(i)
        for j in xrange(p1, p2+1):
            f.insert(i, padding + 'case ' + hex(j) + ':')
            i += 1
        i -= 1
        f[i] += suffix
    i += 1

open(fname, 'wb').write('\n'.join(f))
