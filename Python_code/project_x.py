# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 17:28:11 2020

@author: Marta
"""

def project_x(x_new, X, gamma, alphas, lambdas):
    import numpy as np
    pair_dist = np.array([np.sum((x_new-row)**2) for row in X])
    k = np.exp(-gamma * pair_dist)
    return k.dot(alphas / lambdas)