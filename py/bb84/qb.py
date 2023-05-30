from __future__ import annotations
from random import choice, choices, sample
from copy import deepcopy as cp

import numpy as np

class Qb:
    """Qubit definitions and methods"""

    basis_vec = {
        "0": np.matrix([[1], [0]]),
        "1": np.matrix([[0], [1]]),
        "+": np.matrix([[1 / np.sqrt(2)], [1 / np.sqrt(2)]]),
        "-": np.matrix([[1 / np.sqrt(2)], [-1 / np.sqrt(2)]]),
    }
    gates = {
        "X": np.matrix([[0, 1], [1, 0]]),
        "Y": np.matrix([[0, complex("-j")], [complex("j"), 0]]),
        "Z": np.matrix([[1, 0], [0, -1]]),
        "H": np.matrix(
            [[1 / np.sqrt(2), 1 / np.sqrt(2)], [1 / np.sqrt(2), -1 / np.sqrt(2)]]
        ),
    }
    projectors = {
        "Z": [
            np.matrix(np.outer(basis_vec["0"], basis_vec["0"])),
            np.matrix(np.outer(basis_vec["1"], basis_vec["1"])),
        ],
        "X": [
            np.matrix(np.outer(basis_vec["+"], basis_vec["+"])),
            np.matrix(np.outer(basis_vec["-"], basis_vec["-"])),
        ],
    }

    def __init__(self, coef: complex, basis: str) -> None:
        self.coef = complex(coef)
        self.basis = basis  # for cosmetics
        self.basis_vec = Qb.basis_vec[basis]

    @classmethod
    def amp_prob(cls, qb1: Qb, qb2: Qb) -> float:
        return float(
            np.abs(qb1.coef * qb2.coef) * np.dot(qb1.T.conjugate(), qb2.basis_vec)
        )

    @classmethod
    def prob(cls, qb1: Qb, qb2: Qb) -> float:
        return Qb.amp_prob(qb1, qb2) ** 2

    @classmethod
    def act(cls, gate: np.matrix, qb: Qb) -> None:
        qb.basis_vec = Qb.gates[gate] @ qb.basis_vec
        Qb._basis_update(qb)

    @classmethod
    def measure(cls, qb: Qb, x: Qb.projectors) -> None:
        probs, projections = [], []
        for i in [0, 1]:
            probs.append(
                float(qb.basis_vec.T.conjugate() @ Qb.projectors[x][i] @ qb.basis_vec)
            )
            if probs[i]:
                projections.append(
                    (Qb.projectors[x][i] @ qb.basis_vec) / np.sqrt(probs[i])
                )
            else:
                projections.append(
                    np.matrix([[0.0], [0.0]])
                )  # to have the same lenght as probs

        qb.basis_vec = choices(projections, weights=probs)[0]
        Qb._basis_update(qb)

    @classmethod
    def _basis_update(cls, qb: Qb) -> None:
        """For cosmetics"""
        for base, vec in Qb.basis_vec.items():
            if np.array_equiv(qb.basis_vec, Qb.basis_vec[base] * (-1)):
                qb.basis = base
                qb.basis_vec = qb.basis_vec * -1
                qb.coef = qb.coef * -1

    def __eq__(self, qb: Qb) -> bool:
        if self.coef == qb.coef and np.array_equal(self.basis_vec, qb.basis_vec):
            return True
        return False

    def __repr__(self):
        return f"|{self.basis}>"

    def __str__(self):
        return f"|{self.basis}>"
