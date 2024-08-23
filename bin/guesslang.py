from dataclasses import asdict, dataclass
from typing import TextIO

from langdetect import detect_langs


@dataclass
class Estimate:
    lang: str
    prob: float


class Estimates(list[Estimate]):
    def filter_by_langs(self, langs: list[str]) -> "Estimates":
        return Estimates([x for x in self if x.lang in langs])

    def top(self, default: str) -> Estimate:
        if len(self) == 0:
            return Estimate(lang=default, prob=1)
        return sorted(self, key=lambda x: x.prob, reverse=True)[0]


def guess(src: TextIO) -> Estimates:
    return Estimates([Estimate(lang=x.lang, prob=x.prob) for x in detect_langs(src.read())])


if __name__ == "__main__":
    import argparse
    import json
    import sys
    from textwrap import dedent

    parser = argparse.ArgumentParser(
        prog="guesslang",
        description=dedent(
            """\
            Guess the language of the text from stdin.
            """,
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("filters", nargs="*", help="Only keep results in the specified langs")
    parser.add_argument("--top", action="store_true", help="Only keep the most likely guessed result")
    parser.add_argument("--default", default="unknown", type=str, help="The lang in case there are no results")
    args = parser.parse_args()

    estimates = guess(sys.stdin)
    if len(args.filters) > 0:
        estimates = estimates.filter_by_langs(args.filters)
    if args.top:
        estimates = Estimates([estimates.top(args.default)])
    result = json.dumps([asdict(x) for x in estimates])
    print(result, end="")
