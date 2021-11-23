import readline
import atexit
import os
from functools import wraps
from pyclbr import readmodule_ex
from inspect import (
    getmembers,
    getsource,
    getdoc,
    getfile,
    getclasstree,
    isfunction,
    ismethod,
    signature,
)
from pprint import pprint


readline.parse_and_bind("tab: complete")
histfile = os.path.join(os.environ["PYTHONHISTORY"])
try:
    readline.read_history_file(histfile)
    readline.set_history_length(500)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)


class devutil:
    """Utilities for dev."""

    @staticmethod
    def ignore_exception(f):
        """Ignore any exceptions raised by f."""
        @wraps(f)
        def inner(*args, **kwargs):
            try:
                return f(*args, **kwargs)
            except Exception as e:
                print("{}({}, {}) caused {}".format(f.__name__, args, kwargs, e))
                return None
        return inner

    @staticmethod
    def describe_function_or_method(x):
        """Describe x's methods or functions."""
        for _, f in sorted(getmembers(x, isfunction) + getmembers(x, ismethod)):
            print("{}{}".format(f.__name__, signature(f)))
            print("  " + f.__doc__)


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
    def c(x):
        """Print source of x."""
        print(getsource(x))

    @classmethod
    @devutil.ignore_exception
    def cb(cls, module: str):
        """Print class browser of module."""
        for v in readmodule_ex(module).values():
            cls.pp(vars(v))

    @staticmethod
    @devutil.ignore_exception
    def d(x):
        """Print docstring of x."""
        print(getdoc(x))

    @staticmethod
    @devutil.ignore_exception
    def f(x):
        """Less source of x."""
        os.system("less " + getfile(x))

    @classmethod
    @devutil.ignore_exception
    def g(cls, x):
        """Get members using inspect. Help is available by property h."""
        return cls.InspectMembers(x)

    @staticmethod
    @devutil.ignore_exception
    def pp(x):
        """Pretty print."""
        pprint(x, width=120, depth=None)

    @staticmethod
    @devutil.ignore_exception
    def s(x):
        """Print signature of x."""
        print(signature(x))

    @staticmethod
    @devutil.ignore_exception
    def t(x):
        """Get class tree."""
        return getclasstree([type(x)])

    class InspectMembers:
        def __init__(self, x):
            self.__members = dict(getmembers(x))

        @property
        def h(self):
            devutil.describe_function_or_method(self)

        @property
        def all(self) -> dict:
            """Get all members."""
            return self.__members

        @property
        def keys(self) -> list:
            """Get keys of members."""
            return list(self.__members.keys())

        def get(self, key: str):
            """Get a member by key."""
            return self.__members.get(key)


del histfile, atexit, readline
print(dev.__doc__)
