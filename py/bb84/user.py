from __future__ import annotations
from random import choice

import numpy as np

from .qb import Qb
from .comms import Comms

class User:
    def __init__(self):
        self.bit_choice = list()
        self.base_choice = list()
        self.states = list()
        self.key = list()

    def stream_gen(self, nbits: int) -> None:
        """generates the random qbits to be sended"""
        for i in range(nbits):
            bit = choice(["0", "1", "+", "-"])
            self.bit_choice.append(bit)
            self.states.append(Qb(1, bit))
            base = choice(["X", "Z"])
            self.base_choice.append(base)
            Qb.measure(self.states[i], base)

    def measure(self, stream: list[Qb]) -> None:
        self.states = stream

        for i, j in enumerate(self.states):
            base = choice(["X", "Z"])
            self.base_choice.append(base)
            Qb.measure(self.states[i], base)

    def key_gen(self, x: User) -> None:
        # compares basis choice
        cc = Comms.choice_check(self, x)

        for i in range(len(cc)):
            if cc[i] == 1:
                if np.array_equal(
                    self.states[i].basis_vec, Qb.basis_vec["0"]
                ) or np.array_equal(self.states[i].basis_vec, Qb.basis_vec["+"]):
                    self.key.append(0)
                else:
                    self.key.append(1)
