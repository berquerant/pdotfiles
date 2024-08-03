import atexit
import os
import readline
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

readline.parse_and_bind("tab: complete")
histfile = os.path.join(os.environ["PYTHONHISTORY"])
try:
    readline.read_history_file(histfile)
    readline.set_history_length(1000)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)


class devutil:
    """Utilities for dev."""

    class color(Enum):
        end = "\033[0m"
        red = "\033[31m"
        green = "\033[32m"

        def add(self, s):
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
                msg = "{}({}, {}) caused {}".format(f.__name__, args, kwargs, e)
                print(cls.color.red.add(msg))
                return None

        return inner

    @classmethod
    def describe_function_or_method(cls, x):
        """Describe x's methods or functions."""
        for _, member in sorted(getmembers(x, isfunction) + getmembers(x, ismethod)):
            msg = "{}{}".format(member.__name__, signature(member))
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
    def c(x):
        """Print source of x."""
        print(getsource(x))

    @classmethod
    @devutil.ignore_exception
    def cb(cls, module):
        """
        Print class browser of module.
        :param: module str
        """
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
        msg = "file {} line {} to {}".format(srcfile, start_linum, end_linum)
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

    @classmethod
    @devutil.ignore_exception
    def g(cls, x):
        """Get members using inspect. Help is available by property h."""
        return cls.InspectMembers.new(x)

    @staticmethod
    @devutil.ignore_exception
    def pp(x):
        """Pretty print."""
        pprint(x, width=120, depth=None)

    @staticmethod
    @devutil.ignore_exception
    def s(x):
        """Print signature of x."""
        msg = "{}{}".format(x.__name__, signature(x))
        print(devutil.color.green.add(msg))

    @staticmethod
    @devutil.ignore_exception
    def t(x):
        """Get class tree."""
        return getclasstree([type(x)])

    class InspectMembers:
        def __init__(self, members):
            self.members = members

        @staticmethod
        def new(x):
            return dev.InspectMembers(members=dict(getmembers(x)))

        @property
        def h(self):
            """Print help."""
            devutil.describe_function_or_method(self)

        @property
        def all(self):
            """
            Get all members.
            :return: dict
            """
            return self.members

        @property
        def keys(self):
            """Get keys of members."""
            return list(self.members.keys())

        def get(self, key):
            """Get a member by key."""
            return self.members.get(key)

    @classmethod
    def hashable(cls, x):
        """
        Get hashable.
        :return: Hasable
        """
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
            s = signature(f)
            bindings = ",".join("{}={}".format(k, v) for k, v in s.bind(*args, **kwargs).arguments.items())
            msg = "[{}] call {}{}".format(rid, f.__name__, bindings)
            print(msg)
            r = f(*args, **kwargs)
            msg = "[{}] Returned {}".format(rid, r)
            print(msg)
            return r

        return wrapper


del histfile, atexit, readline
print(devutil.color.green.add(dev.__doc__))
