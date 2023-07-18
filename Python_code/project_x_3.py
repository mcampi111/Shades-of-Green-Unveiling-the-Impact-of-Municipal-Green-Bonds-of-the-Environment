# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 17:28:11 2020

@author: Marta
"""

def project_x_3(x_new_2, X2, gamma, alphas, lambdas):
    import numpy as np
    from scipy.spatial.distance import pdist, jaccard
    pair_dist_2 = np.array([np.sum(1 - pdist(np.c_[x_new_2,row2], 'jaccard')) for row2 in X2])
    pair_dist3 = pair_dist_2
    k = np.exp(-gamma * pair_dist3)
    return k.dot(alphas / lambdas)