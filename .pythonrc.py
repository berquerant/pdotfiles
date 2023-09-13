import atexit
import os
import readline
from dataclasses import dataclass
from enum import Enum
from functools import wraps
from inspect import (getclasstree, getdoc, getfile, getmembers, getsource,
                     getsourcelines, isfunction, ismethod, signature)
from pprint import pprint
from pyclbr import readmodule_ex
from typing import (Any, Callable, ClassVar, Hashable, Optional, ParamSpec,
                    TypeVar)
from uuid import uuid4

readline.parse_and_bind("tab: complete")
histfile = os.path.join(os.environ["PYTHONHISTORY"])
try:
    readline.read_history_file(histfile)
    readline.set_history_length(500)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)


T = TypeVar("T")
D = TypeVar("D")
P = ParamSpec("P")


class devutil:
    """Utilities for dev."""

    class color(Enum):
        end = "\033[0m"
        red = "\033[31m"
        green = "\033[32m"

        def add(self, s: str) -> str:
            """Add color to the string."""
            return self.value + s + self.end.value

    @classmethod
    def ignore_exception(cls, f: Callable[P, T]) -> Callable[P, T | None]:
        """Ignore any exceptions raised by f."""

        @wraps(f)
        def inner(*args: P.args, **kwargs: P.kwargs) -> T | None:
            try:
                return f(*args, **kwargs)
            except Exception as e:
                print(cls.color.red.add(f"{f.__name__}({args}, {kwargs}) caused {e}"))
                return None

        return inner

    @classmethod
    def describe_function_or_method(cls, x: Any) -> None:
        """Describe x's methods or functions."""
        for _, member in sorted(getmembers(x, isfunction) + getmembers(x, ismethod)):
            print(cls.color.green.add(f"{member.__name__}{signature(member)}"))
            if member.__doc__:
                print("  " + member.__doc__)


class dev:
    """Tools for python interpreter are available. Print help by dev.h()."""

    FILE: ClassVar[str] = __file__

    @classmethod
    @devutil.ignore_exception
    def h(cls) -> None:
        """Print help."""
        print(cls.__doc__)
        devutil.describe_function_or_method(cls)

    @staticmethod
    @devutil.ignore_exception
    def c(x: Any) -> None:
        """Print source of x."""
        print(getsource(x))

    @classmethod
    @devutil.ignore_exception
    def cb(cls, module: str) -> None:
        """Print class browser of module."""
        for v in readmodule_ex(module).values():
            cls.pp(vars(v))

    @staticmethod
    @devutil.ignore_exception
    def d(x: Any) -> None:
        """Print docstring of x."""
        print(getdoc(x))

    @staticmethod
    @devutil.ignore_exception
    def l(x: Any) -> None:
        """Print the location in which x was defined in."""
        srcfile = getfile(x)
        lines, start_linum = getsourcelines(x)
        end_linum = start_linum + len(lines) - 1
        print(f"file {srcfile} line {start_linum} to {end_linum}")

    @staticmethod
    @devutil.ignore_exception
    def f(x: Any, n: bool = False) -> None:
        """Less source of x. Display line number if n is True."""
        cmd = ["less"]
        if n:
            cmd.append("-N")
        cmd.append(getfile(x))
        os.system(" ".join(cmd))

    @classmethod
    @devutil.ignore_exception
    def g(cls, x: Any):
        """Get members using inspect. Help is available by property h."""
        return cls.InspectMembers.new(x)

    @staticmethod
    @devutil.ignore_exception
    def pp(x: Any) -> None:
        """Pretty print."""
        pprint(x, width=120, depth=None)

    @staticmethod
    @devutil.ignore_exception
    def s(x: Any) -> None:
        """Print signature of x."""
        print(devutil.color.green.add(f"{x.__name__}{signature(x)}"))

    @staticmethod
    @devutil.ignore_exception
    def t(x: Any) -> Any:
        """Get class tree."""
        return getclasstree([type(x)])

    @dataclass
    class InspectMembers:
        members: dict[str, Any]

        @staticmethod
        def new(x: Any):
            return dev.InspectMembers(members=dict(getmembers(x)))

        @property
        def h(self) -> None:
            """Print help."""
            devutil.describe_function_or_method(self)

        @property
        def all(self) -> dict:
            """Get all members."""
            return self.members

        @property
        def keys(self) -> list:
            """Get keys of members."""
            return list(self.members.keys())

        def get(self, key: str) -> Any:
            """Get a member by key."""
            return self.members.get(key)

    @classmethod
    def hashable(cls, x: Any) -> Hashable:
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
    def w(f: Callable[P, T]) -> Callable[P, T]:
        """Watch the arguments and the return value."""

        @wraps(f)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
            rid = uuid4()
            s = signature(f)
            bindings = ",".join(
                f"{k}={v}" for k, v in s.bind(*args, **kwargs).arguments.items()
            )
            print(f"[{rid}] Call {f.__name__}{bindings}")
            r = f(*args, **kwargs)
            print(f"[{rid}] Returned {r}")
            return r

        return wrapper


del histfile, atexit, readline
print(devutil.color.green.add(dev.__doc__))
