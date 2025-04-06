from typing import Any
from abc import ABC, abstractmethod
import json
import sys
import textwrap
from datetime import datetime
import os
from dataclasses import dataclass, field
from unstructured.partition.auto import partition
import requests
from tempfile import NamedTemporaryFile


class Base(ABC):
    @abstractmethod
    def name(self) -> str: ...
    @abstractmethod
    def desc(self) -> str: ...
    @abstractmethod
    def props(self) -> dict[str, Any]: ...
    @abstractmethod
    def run(self, v: dict[str, Any]) -> int: ...

    def properties2schema(self) -> dict[str, Any]:
        return {
            "title": "Args",
            "type": "object",
            "properties": self.props(),
            "additionalProperties": False,
            "required": list(self.props().keys()),
        }

    @staticmethod
    def dumps(v: Any) -> None:
        print(json.dumps(v, separators=(",", ":"), ensure_ascii=False))

    def main(self, input: str) -> int:
        try:
            obj = json.loads(input)
        except Exception:
            print(
                json.dumps(
                    {
                        "name": self.name(),
                        "description": self.desc(),
                        "schema": self.properties2schema(),
                    }
                )
            )
            return 2
        return self.run(obj)


class CurrentLocalTime(Base):
    def name(self) -> str:
        return "current_local_time"

    def desc(self) -> str:
        return textwrap.dedent("""\
        Display the current local time.
        Returns:
            time: string like '2024-01-02 12:13:14'""")

    def props(self) -> dict[str, Any]:
        return {}

    def run(self, v: dict[str, Any]) -> int:
        now = datetime.now()
        self.dumps({"time": now.strftime("%Y-%m-%d %H:%M:%S")})
        return 0


class FetchFromWeb(Base):
    def name(self) -> str:
        return "fetch_from_web"

    def desc(self) -> str:
        return textwrap.dedent("""\
        Get the document from web.
        Args:
            url: location of document
        Returns:
            text: text of document
        """)

    def props(self) -> dict[str, Any]:
        return {
            "url": {
                "title": "url",
                "type": "string",
            },
        }

    def run(self, v: dict[str, Any]) -> int:
        try:
            self.dumps({"text": self.do(v["url"])})
            return 0
        except Exception as e:
            print(e, file=sys.stderr)
            return 1

    @staticmethod
    def do(url: str) -> str:
        r = requests.get(url)
        if r.status_code != 200:
            raise Exception(f"GET {url} caused status code {r.status_code}")
        with NamedTemporaryFile(mode="w") as f:
            print(r.text, file=f)
            f.seek(0)
            ps = partition(f.name, request_timeout=30)
            return "\n\n".join(x.text for x in ps)


def tools() -> dict[str, Base]:
    return {
        x.name(): x
        for x in [
            CurrentLocalTime(),
            FetchFromWeb(),
        ]
    }


@dataclass
class Generator:
    droot: str
    names: list[str] = field(default_factory=list)

    @property
    def root(self) -> str:
        x = f"{self.droot}/tmp/my-ai-agent"
        os.makedirs(x, exist_ok=True)
        return x

    def generate_script(self, name: str) -> str:
        return textwrap.dedent(
            '''\
        #!/bin/bash
        "${{PYENV_ROOT}}/shims/python" "${{DOTFILES_ROOT}}/bin/my-ai-agent-tools.py" {name} "$@"'''.format(name=name)
        )

    def generate(self) -> int:
        for name in self.names:
            s = self.generate_script(name)
            p = self.root + "/" + name + ".sh"
            print(f"generate {p}...")
            with open(p, "w") as f:
                print(s, file=f)
            os.chmod(p, 0o755)
        return 0


def main(args: list[str]) -> int:
    if len(args) != 2:
        print("only 1 arg is allowed", file=sys.stderr)
        return 1
    name = args[1]
    if name == "generate":
        droot = os.getenv("DOTFILES_ROOT")
        if droot is None:
            print("$DOTFILES_ROOT is not set", file=sys.stderr)
            return 1
        return Generator(droot=droot, names=list(tools().keys())).generate()

    t = tools().get(name)
    if t is None:
        names = "\b".join(tools().keys())
        print(f"{name} is not found; available:\n{names}", file=sys.stderr)
        return 1

    return t.main(sys.stdin.read())


if __name__ == "__main__":
    sys.exit(main(sys.argv))
