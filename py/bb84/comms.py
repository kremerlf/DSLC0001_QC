from __future__ import annotations
from random import choice, sample
from copy import deepcopy as cp

from .qb import Qb

class Comms:
    """Public communication channel"""

    @classmethod
    def send(cls, x: User) -> list[Qb]:
        return cp(x.states)

    @classmethod
    def choice_check(cls, x: User, y: User) -> list[int]:
        return [
            1 if x.base_choice[i] == y.base_choice[i] else 0
            for i, j in enumerate(x.base_choice)
        ]

    @classmethod
    def random_key_test(cls, x: User, y: User) -> tuple[int, str]:
        key_sample = sample(range(len(x.key)), int(len(x.key) / 2))
        for i in key_sample:
            if x.key[i] != y.key[i]:
                return (1, "Got noise!!!")
        return (0, "all ok")
