from typing import Any
from abc import ABC, abstractmethod
import json
import sys
import textwrap
from datetime import datetime
import os
from dataclasses import dataclass, field
from unstructured.partition.auto import partition
from tavily import TavilyClient


def log(msg: str) -> None:
    print(msg, file=sys.stderr)


class Base(ABC):
    @abstractmethod
    def name(self) -> str: ...
    @abstractmethod
    def desc(self) -> str: ...
    @abstractmethod
    def props(self) -> dict[str, Any]: ...
    @abstractmethod
    def run(self, v: dict[str, Any]) -> int: ...
    @property
    def schema(self) -> dict[str, Any]:
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
            self.dumps(
                {
                    "name": self.name(),
                    "description": self.desc(),
                    "schema": self.schema,
                }
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
            log(str(e))
            return 1

    @staticmethod
    def do(url: str) -> str:
        return "\n\n".join(x.text for x in partition(url=url, request_timeout=60))


class SearchWeb(Base):
    def name(self) -> str:
        return "search_web"

    def desc(self) -> str:
        return textwrap.dedent("""\
        Execute a web search query.
        Args:
            query: The search query to execute.
            time_range: The time range back from the current date to filter results. Useful when looking for sources that have published data.
        Returns:
            answer: A short answer to the user's query.
            results: A list of sorted search results, ranked by relevancy.
                title: The title of the search result.
                url: The URL of the search result.
                content: A short description of the search result.
                score: The relevance score of the search result.
        """)

    def props(self) -> dict[str, Any]:
        return {
            "query": {
                "title": "query",
                "type": "string",
            },
            "time_range": {
                "title": "time_range",
                "type": "string",
                "enum": ["day", "week", "month", "year"],
            },
        }

    def run(self, v: dict[str, Any]) -> int:
        self.dumps(self.do(v["query"], v["time_range"]))
        return 0

    @staticmethod
    def client() -> TavilyClient:
        return TavilyClient(os.getenv("TAVILY_API_KEY") or "")

    @classmethod
    def do(cls, query: str, time_range: str) -> dict[str, Any]:
        c = cls.client()
        r = c.search(query=query, time_range=time_range)
        return {
            "answer": r["answer"],
            "results": [
                {k: v for k, v in elem.items() if k in {"title", "url", "content", "score"}} for elem in r["results"]
            ],
        }


def tools() -> dict[str, Base]:
    return {
        x.name(): x
        for x in [
            CurrentLocalTime(),
            FetchFromWeb(),
            SearchWeb(),
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
        log("only 1 arg is allowed")
        return 1
    name = args[1]
    if name == "generate":
        droot = os.getenv("DOTFILES_ROOT")
        if droot is None:
            log("$DOTFILES_ROOT is not set")
            return 1
        return Generator(droot=droot, names=list(tools().keys())).generate()

    t = tools().get(name)
    if t is None:
        names = "\n".join(tools().keys())
        log(f"{name} is not found; available:\n{names}")
        return 1

    return t.main(sys.stdin.read())


if __name__ == "__main__":
    sys.exit(main(sys.argv))
