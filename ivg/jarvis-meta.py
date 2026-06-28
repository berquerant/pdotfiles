from openjarvis.core.registry import ToolRegistry, AgentRegistry
from openjarvis.core.types import ToolResult
from openjarvis.tools._stubs import BaseTool, ToolSpec
from openjarvis.recipes.loader import resolve_recipe
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass, field
from typing import Any
import sys
import os
import subprocess
import tomllib


def now_str() -> str:
    return datetime.now().strftime("%Y%m%d%H%M%S")


def format_obj_list_into_text(key: str, xs: list[dict[str, str]]) -> str:
    ss = []
    for x in xs:
        name = x[key]
        attrs = [f"## {k}\n{x[k]}" for k in sorted(x.keys())]
        ss.append(f"# {name}" + "\n" + "\n".join(attrs))
    return "\n\n".join(ss)


def list_tools():
    """See https://github.com/open-jarvis/OpenJarvis/blob/56c9a59f8dfa138f16afd3ccff5d394a13801162/src/openjarvis/cli/tool_cmd.py#L16"""
    import openjarvis.tools  # load tools

    result = []
    keys = sorted(ToolRegistry.keys())
    for key in keys:
        tool_cls = ToolRegistry.get(key)
        description = ""
        category = ""
        # Try to get spec from the class or an instance
        try:
            # Some tools may require initialization, so try with default init
            tool_instance = tool_cls() if callable(tool_cls) else tool_cls
            if hasattr(tool_instance, "spec"):
                spec = tool_instance.spec
                description = getattr(spec, "description", "")
                category = getattr(spec, "category", "")
        except Exception:
            # If instantiation fails, just show the key
            description = "N/A (instantiation error)"
        result.append(
            {
                "name": key,
                "description": description,
                "category": category,
            }
        )
    return result


def list_agents():
    import openjarvis.agents  # load agents
    from inspect import getdoc

    result = []
    keys = sorted(AgentRegistry.keys())
    for key in keys:
        agent_cls = AgentRegistry.get(key)
        doc = ""
        rawdoc = getdoc(agent_cls)
        if rawdoc is not None:
            doc = rawdoc
        result.append(
            {
                "name": key,
                "description": doc,
            }
        )

    return result


@ToolRegistry.register("list_tools")
class ListTools(BaseTool):
    tool_id = "list_tools"

    @property
    def spec(self) -> ToolSpec:
        return ToolSpec(
            name="list_tools",
            description="List all registered tools with their descriptions.",
            parameters={
                "type": "object",
                "properties": {},
                "required": [],
            },
            category="custom",
        )

    def execute(self, **params) -> ToolResult:
        return ToolResult(
            tool_name="list_tools",
            content=format_obj_list_into_text("name", list_tools()),
            success=True,
        )


@ToolRegistry.register("list_agents")
class ListAgents(BaseTool):
    tool_id = "list_agents"

    @property
    def spec(self) -> ToolSpec:
        return ToolSpec(
            name="list_agents",
            description="List all registered agents with their descriptions.",
            parameters={
                "type": "object",
                "properties": {},
                "required": [],
            },
            category="custom",
        )

    def execute(self, **params) -> ToolResult:
        return ToolResult(
            tool_name="list_agents",
            content=format_obj_list_into_text("name", list_agents()),
            success=True,
        )


def json_dumps(x):
    import json

    return json.dumps(x, separators=(",", ":"))


def _format_obj_list(x, out: str) -> str:
    match out:
        case "json":
            return json_dumps(x)
        case _:
            return format_obj_list_into_text("name", x)


def list_agents_cmd(args):
    print(_format_obj_list(list_agents(), args.out))


def list_tools_cmd(args):
    print(_format_obj_list(list_tools(), args.out))


def list_compositions_cmd(args):
    subprocess.run(["jarvis-run", "jarvis", "compose", "list"], check=True)


@dataclass
class Script:
    agent: str
    prompt: str
    tools: list[str] = field(default_factory=list)

    @classmethod
    def create(cls, obj: dict[str, Any]):
        if "agent" not in obj:
            raise Exception("Script: no agent!")
        agent = obj["agent"]
        if not isinstance(agent, str):
            raise Exception(f"Script: invalid agent: {agent}")
        if "prompt" not in obj:
            raise Exception("Script: no prompt!")
        prompt = obj["prompt"]
        if not isinstance(prompt, str):
            raise Exception(f"Script: invalid prompt: {prompt}")
        if "tools" not in obj:
            raise Exception("Script: no tools")
        tools = obj["tools"]
        if not isinstance(tools, list) or not all(isinstance(x, str) for x in tools):
            raise Exception("Script: invalid tools: {tools}")
        return cls(agent=agent, prompt=prompt, tools=tools)

    def run(self, engine: str, model: str, query: str = "") -> str:
        from openjarvis import Jarvis

        j = Jarvis(model=model, engine_key=engine)
        try:
            result = j.ask_full(
                self.prompt + query,
                agent=self.agent,
                tools=self.tools,
            )
            response = result["content"]
        except Exception as exc:
            raise Exception("Error during asking") from exc
        finally:
            j.close()
        return response


def gen_cmd(args):
    prompt = '''# あなたの役割
あなたは、ユーザーが指定した「目的」を達成するために最適な「AIアシスタントの設計図（設定）」を定義するメタ・エージェントです。
与えられた目的を深く分析し、それを最も効率的かつ高い精度で実行できる `agent`、`tools`、および `system_prompt` の組み合わせを考案し、指定されたTOMLフォーマットで出力してください。

# 思考プロセスと手順
1. **目的の分解**: ユーザーが求める目的（例: 「コードレビュー」「ログ解析」など）を達成するために、どのような思考プロセスが必要かを分解します。
2. **コンポーネントの選定**:
   - 目的を達成するために必要なツールを、利用可能なリストから選択します。
   - その処理に最適なエージェントの型（ReAct、Plan-and-Executeなど）を選択します。
3. **プロンプトの設計**:
   - 生成するプロンプトには、「役割」「制約事項」「出力フォーマット」を必ず含めてください。

# 制約事項
- 利用可能な `agent.type` および `agent.tools` は、それぞれ `list_agents` と `list_tools` から得られるもの**のみ**を、そのままの名前で使用してください。架空のツールやエージェントを捏造してはいけません。
- 出力は指定されたTOMLフォーマットのみとし、挨拶、解説、補足、お節介なアドバイスは一切含めないでください。
- 出力は生成したTOMLの内容そのもののみとします。例えば出力がTOMLであることを示すためのコードブロックは不要です。
- 生成される `system_prompt` は、ターゲットとなるAIアシスタントが迷わず動けるよう、極めて具体的かつ論理的に記述してください。
- 生成されるアシスタントの名前、 `recipe.name` は小文字アルファベットとハイフンのみ使用可能です。

# 出力フォーマット
必ず以下のTOML形式のみで出力してください。

[recipe]
name = "[アシスタントの名前]"
description = "[アシスタントの簡単な説明]"
version = "0.1.0"

[engine]
key = "ollama"

[intelligence]
model = "gemma4:12b"

[agent]
type = "[list_agents から選んだエージェント名]"
tools = [
    "[list_tools から選んだツール名1]",
    "[list_tools から選んだツール名2]",
]
system_prompt = """\
# あなたの役割
[ここにこのアシスタントが果たすべき具体的な役割を記述]

# 制約事項
- [アシスタントが遵守すべき制約、トーン＆マナー、禁止事項などを箇条書きで記述]
- [使用すべきツールへの言及（例: 〇〇の実行には XX ツールを使ってください）]

# 出力フォーマット
[アシスタントが出力すべき構造（Markdown等）を明確に定義]
"""

# クエリ
'''
    s = Script(
        tools=["think", "list_tools", "list_agents"],
        agent="orchestrator",
        prompt=prompt + args.query,
    )
    r = s.run(engine=args.engine, model=args.model)
    print(r)
    try:
        t = tomllib.loads(r)
    except Exception as e:
        raise Exception("Generated recipe is invalid toml!") from e
    name = t["recipe"]["name"]
    p = Path(args.recipes) / f"jarvis-meta_{name}_{now_str()}.toml"
    print(f"Write the recipe into {p}", file=sys.stderr)
    with p.open("w") as f:
        print(r, file=f)
    print(f"Check the composition {name} by jarvis-meta compositions")


def chat_cmd(args):
    r = resolve_recipe(args.recipe)
    if r is None:
        raise Exception(f"Recipe {args.recipe} is not found!")
    engine = args.engine or r.engine_key or "ollama"
    model = args.model or r.model or "gemma4:e4b"
    agent = args.agent or r.agent_type or "orchestrator"
    tools = ""
    if args.tools is not None:
        tools = args.tools
    elif r.tools is not None:
        tools = ",".join(r.tools)
    system = args.system or r.system_prompt or ""

    cmd = [
        "jarvis",
        "chat",
        "--engine",
        engine,
        "--model",
        model,
        "--agent",
        agent,
        "--system",
        system,
    ]
    if tools:
        cmd += ["--tools", tools]

    print(f"exec: {cmd}", file=sys.stderr)
    if args.dry:
        return
    sys.stdout.flush()
    sys.stderr.flush()
    os.execvp("jarvis", cmd)


if __name__ == "__main__":
    import argparse
    from os.path import expanduser

    p = argparse.ArgumentParser(
        prog="jarvis-meta",
        description="Generate AI assistant recipe",
    )
    sp = p.add_subparsers(required=True)

    agents = sp.add_parser("agents", help="List agents")
    agents.set_defaults(func=list_agents_cmd)
    agents.add_argument("--out", "-o", choices=["json", "text"], default="text", help="output format")

    tools = sp.add_parser("tools", help="List tools")
    tools.set_defaults(func=list_tools_cmd)
    tools.add_argument("--out", "-o", choices=["json", "text"], default="text", help="output format")

    gen = sp.add_parser("gen", help="Generate AI assistant recipe")
    gen.set_defaults(func=gen_cmd)
    gen.add_argument("--engine", "-e", default="ollama", help="engine backend")
    gen.add_argument("--model", "-m", default="gemma4:12b", help="model to use")
    # https://github.com/open-jarvis/OpenJarvis/blob/main/src/openjarvis/recipes/loader.py#L282
    gen.add_argument("--recipes", "-r", default=expanduser("~/.openjarvis/recipes"))
    gen.add_argument("query", type=str)

    compositions = sp.add_parser("compositions", help="List compositions")
    compositions.set_defaults(func=list_compositions_cmd)

    chat = sp.add_parser("chat", help="Start an interactive multi-turn chat session")
    chat.set_defaults(func=chat_cmd)
    chat.add_argument("--recipe", "-r", required=True, type=str, help="recipe name")
    chat.add_argument("--engine", "-e", type=str, help="engine backend")
    chat.add_argument("--model", "-m", type=str, help="model to use")
    chat.add_argument("--agent", "-a", type=str, help="agent type")
    chat.add_argument("--tools", type=str, help="comma-separated tool names")
    chat.add_argument("--system", type=str, help="custom system prompt")
    chat.add_argument("--dry", action="store_true")

    args = p.parse_args()
    args.func(args)
