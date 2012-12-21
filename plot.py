import numpy
import scipy
import pylab
import re
import sys

def masses_to_indices(ms):
    ix = []
    i = ms[0]
    for m in ms[1:]:
        ix.append(i)
        i += m
    return ix

class Example(object):
    def __init__(self, ref, predicted, score, threshold, w):
        self.ref = ref
        self.predicted = predicted
        self.score = score
        self.threshold = threshold
        self.w = w

    def plot(self):
        pylab.xlabel('Sentence')
        pylab.ylabel('Gap Similarity Score')
        pylab.plot(range(self.w, len(self.score)+self.w), self.score)
        #pylab.axhline(self.threshold, label='Threshold', color='red')
        print len(self.score)
        for x in masses_to_indices(self.ref):
            pylab.axvline(x, color='green', alpha=0.5)
        for x in masses_to_indices(self.predicted):
            pylab.axvline(x, color='red', linestyle='--')
        pylab.grid(True)

exs = []
ref = None
for line in file('log-tdt.txt','r'):
    if line.startswith('Reference: '):
        ref = numpy.array(eval(line[16:]))
    if line.startswith('# TopicTiling'):
        d = eval(line[14:], {'NaN': float('nan')})
        exs.append(Example(ref, **d))

exs[int(sys.argv[1])].plot()
pylab.show()

