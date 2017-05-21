#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt

def source(x):
    """
    Represents a source term.
    """
    return x * x

def psi(i, xi):
    """
    1D Lagrange basis.
    """
    if i == 0:
        return 0.5*(1.0 - xi)
    elif i == 1:
        return 0.5*(1.0 + xi)
    else:
        print("Error: i must be 0 or 1.")
        sys.exit()

def dpsi(i, xi):
    """
    1D Lagrange basis.
    """
    if i == 0:
        return -0.5
    elif i == 1:
        return 0.5
    else:
        print("Error: i must be 0 or 1.")
        sys.exit()

def interpSource(f, xi, x):
    """
    Function for interpolating a source term using the basis.
    """
    interpVal = 0.0
    for i in range(2):
        interpVal += psi(i, xi) * f(x[i])
    return interpVal

def integrand(xi, i, j):
    return dpsi(i, xi) * dpsi(j, xi) + psi(i, xi) * psi(j, xi)

# Interpolating
nodalCoords = [0.4, 0.5]
intSource = []
intSource.append(interpSource(source, -1.0, nodalCoords))
intSource.append(interpSource(source, 1.0, nodalCoords))

# Plotting
xvec = np.linspace(0.3, 0.6, 30)
yvec = source(xvec)
plt.plot(xvec,yvec)
plt.plot(nodalCoords, intSource, "--r")

# Computing entries in the stiffness matrix
x1 = 0.2
x2 = 0.4
detJ = (x2 - x1)/2.0
gpts = [0.0]
gwgts = [2.0]
gpts = [-0.577350269189626, 0.577350269189626]
gwgts = [1.0, 1.0]
K = np.zeros((2, 2))
for gpt, gw in zip(gpts, gwgts):
    for i in range(2):
        for j in range(2):
            K[i, j] += gw*integrand(gpt, i, j)

K *= detJ

print(K)

#plt.show()

