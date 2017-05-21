#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt

def exact(x):
    c1 = (2.0 - 3.0*np.exp(1.0))/(np.exp(2.0) - 1.0)
    c2 = (3.0 - 2.0*np.exp(1.0))*np.exp(1.0)/(np.exp(2.0) - 1.0)
    return c1*np.exp(x) + c2*np.exp(-x) + x*x + 2.0
    #c1 = -np.exp(1.0)/(np.exp(-1.0) - np.exp(1.0))
    #c2 = np.exp(1.0)/(np.exp(-1.0) - np.exp(1.0))
    #return c1*np.exp(x) + c2*np.exp(-x)

data = np.genfromtxt("linear.dat")
x_exact = np.linspace(0.0, 1.0, 100)
f_exact = exact(x_exact)
x_num = data[:, 0]
f_num = data[:, 1]
plt.plot(x_exact, f_exact, "-k", lw=1.5, label="Exact")
plt.plot(x_num, f_num, "--r", lw=1.5, label="Numerical")
plt.legend()
plt.show()
