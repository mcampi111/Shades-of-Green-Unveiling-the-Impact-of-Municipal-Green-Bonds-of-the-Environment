# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 11:29:48 2020

@author: Marta
"""

def stepwise_kpca_2(K_norm, n_components):
    import numpy as np
    from scipy.linalg import eigh
    

    # Obtaining eigenvalues in descending order with corresponding
    # eigenvectors from the symmetric matrix.
    eigvals, eigvecs = eigh(K_norm)

    # Obtaining the i eigenvectors (alphas) that corresponds to the i highest eigenvalues (lambdas).
    alphas = np.column_stack((eigvecs[:,-i] for i in range(1,n_components+1)))
    lambdas = [eigvals[-i] for i in range(1,n_components+1)]

    return alphas, lambdas