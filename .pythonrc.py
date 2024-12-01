import json
import os
from decimal import Decimal
from enum import Enum
from functools import wraps
from inspect import (
    getclasstree,
    getdoc,
    getfile,
    getmembers,
    getsource,
    getsourcelines,
    isfunction,
    ismethod,
    signature,
)
from pprint import pprint
from pyclbr import readmodule_ex
from uuid import uuid4

import yaml


class devutil:
    """Utilities for dev."""

    @staticmethod
    def qualname(x) -> str:
        s = []
        if hasattr(x, "__module__"):
            s.append(x.__module__)
        if hasattr(x, "__qualname__"):
            s.append(x.__qualname__)
        return ".".join(s)

    @classmethod
    def dump_yaml(cls, x) -> str:
        return yaml.dump(cls.dumpable(x), allow_unicode=True, indent=2)

    @classmethod
    def dump_json(cls, x, indent=False) -> str:
        args = {
            "obj": cls.dumpable(x),
            "ensure_ascii": False,
            "sort_keys": True,
        }
        if indent:
            args["indent"] = 2
        else:
            args["separators"] = (",", ":")
        return json.dumps(**args)

    @classmethod
    def dumpable(cls, x):
        match x:
            case list() | tuple() | set() | frozenset():
                return [cls.dumpable(a) for a in x]
            case dict():
                return {cls.dumpable(k): cls.dumpable(v) for k, v in x.items()}
            case bytearray():
                return cls.dumpable(bytes(x))
            case bytes():
                return x.decode()
            case str() | int() | float() | bool():
                return x
            case Decimal():
                return float(x)
            case _:
                return str(x)

    class color(Enum):
        end = "\033[0m"
        red = "\033[31m"
        green = "\033[32m"

        def add(self, s: str) -> str:
            """Add color to the string."""
            return self.value + s + self.end.value

    @classmethod
    def ignore_exception(cls, f):
        """Ignore any exceptions raised by f."""

        @wraps(f)
        def inner(*args, **kwargs):
            try:
                return f(*args, **kwargs)
            except Exception as e:
                n = cls.qualname(f)
                msg = f"{n}({args}, {kwargs}) caused {e}"
                print(cls.color.red.add(msg))
                return None

        return inner

    @classmethod
    def describe_function_or_method(cls, x):
        """Describe x's methods or functions."""
        for _, member in sorted(getmembers(x, isfunction) + getmembers(x, ismethod)):
            n = member.__qualname__
            msg = f"{n}{signature(member)}"
            print(cls.color.green.add(msg))
            if member.__doc__:
                print("  " + member.__doc__)


class dev:
    """Tools for python interpreter are available. Print help by dev.h()."""

    FILE = __file__

    @classmethod
    @devutil.ignore_exception
    def h(cls):
        """Print help."""
        print(cls.__doc__)
        devutil.describe_function_or_method(cls)

    @staticmethod
    @devutil.ignore_exception
    def q(x):
        """Print qualified name of x."""
        print(devutil.qualname(x))

    @staticmethod
    @devutil.ignore_exception
    def j(x, indent=False):
        """Print json string."""
        print(devutil.dump_json(x, indent=indent))

    @staticmethod
    @devutil.ignore_exception
    def y(x):
        """Print yaml string."""
        print(devutil.dump_yaml(x))

    @staticmethod
    @devutil.ignore_exception
    def c(x):
        """Print source of x."""
        print(getsource(x))

    @classmethod
    @devutil.ignore_exception
    def cb(cls, module):
        """Print class browser of module(str)."""
        for v in readmodule_ex(module).values():
            cls.pp(vars(v))

    @staticmethod
    @devutil.ignore_exception
    def d(x):
        """Print docstring of x."""
        print(getdoc(x))

    @staticmethod
    @devutil.ignore_exception
    def l(x):
        """Print the location in which x was defined in."""
        srcfile = getfile(x)
        lines, start_linum = getsourcelines(x)
        end_linum = start_linum + len(lines) - 1
        msg = f"file {srcfile} line {start_linum} to {end_linum}"
        print(msg)

    @staticmethod
    @devutil.ignore_exception
    def f(x, n=False):
        """Less source of x. Display line number if n is True."""
        cmd = ["less"]
        if n:
            cmd.append("-N")
        cmd.append(getfile(x))
        os.system(" ".join(cmd))

    @staticmethod
    @devutil.ignore_exception
    def i(x):
        """Inspect x."""
        return dict(getmembers(x))

    @staticmethod
    @devutil.ignore_exception
    def pp(x):
        """Pretty print."""
        pprint(x, width=120, depth=None)

    @staticmethod
    @devutil.ignore_exception
    def s(x):
        """Print signature of x."""
        n = devutil.qualname(x)
        msg = f"{n}{signature(x)}"
        print(devutil.color.green.add(msg))

    @staticmethod
    @devutil.ignore_exception
    def t(x):
        """Get class tree."""
        return getclasstree([type(x)])

    @classmethod
    def hashable(cls, x):
        """Get hashable."""
        match x:
            case list() | tuple() | set() | frozenset():
                return tuple(cls.hashable(z) for z in x)
            case dict():
                return tuple((k, cls.hashable(x[k])) for k in sorted(x))
            case bytearray():
                return bytes(x)
            case _:
                return x

    @staticmethod
    def w(f):
        """Watch the arguments and the return value."""

        @wraps(f)
        def wrapper(*args, **kwargs):
            rid = uuid4()
            name = devutil.qualname(f)
            s = signature(f)
            bindings = ",".join(f"{k}={v}" for k, v in s.bind(*args, **kwargs).arguments.items())
            print(f"[{rid}] call {name}({bindings})")
            try:
                r = f(*args, **kwargs)
                print(f"[{rid}] returned({r})")
                return r
            except Exception as e:
                print(f"[{rid}] exception({e})")
                raise e

        return wrapper


print(devutil.color.green.add(dev.__doc__))
