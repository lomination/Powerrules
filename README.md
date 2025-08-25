<div align="center">

[![Version badge](https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Flomination%2F332a8f012e03423e217c300418f37718%2Fraw%2Fpowerrules-version-badge.json%3Fcachebust%3Ddkjflskjfldkf)
](https://github.com/lomination/Powerrules/releases/latest)
[![Build](https://img.shields.io/github/actions/workflow/status/lomination/Powerrules/.github/workflows/build.yaml?branch=main)](https://github.com/lomination/Powerrules/actions/workflows/build.yaml)
[![Check format](https://img.shields.io/github/actions/workflow/status/lomination/Powerrules/.github/workflows/check-format.yaml?branch=main&label=check%20format)](https://github.com/lomination/Powerrules/actions/workflows/check-format.yaml)
[![Check example](https://img.shields.io/github/actions/workflow/status/lomination/Powerrules/.github/workflows/check-examples.yaml?branch=main&label=check%20examples)](https://github.com/lomination/Powerrules/actions/workflows/check-examples.yaml)
<br>
![Line coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Flomination%2F6d46d5386c81a9b73454731cbb5cc358%2Fraw%2Fpowerrules-line-coverage-badge.json%3Fcachebust%3Ddkjflskjfldkf)
![Branch coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Flomination%2F07f82f2d3ab43091376eb29126e2839c%2Fraw%2Fpowerrules-branch-coverage-badge.json%3Fcachebust%3Ddkjflskjfldkf)

</div>

# Powerrules

Powerrules is a language designed to generate rules for [DDNet](https://ddnet.org) ([GitHub](https://github.com/ddnet/ddnet)). Its goal is to simplify writing rules by providing a more user-friendly and powerful syntax.

You can use the [online transpiler](https://lomination.github.io/Powerrules/) to convert Powerrules into DDNet rules. For more information on how to write Powerrules, check out the [wiki](https://github.com/lomination/Powerrules/wiki).

Special thanks to [Toom](https://github.com/To-om) for significant technical help and advice with Scala, Ssor for support and ideas, and [archimede67](https://github.com/archimede67) for fixing [bugs](https://github.com/ddnet/ddnet/issues/8134) that almost led me to abandon the project!

## Why Powerrules?

Writing rules for the DDNet automapper is often tricky. It can be confusing because of its format which is neither readable nor intuitive. Moreover, writing rules is often about writing the same patterns: first making grass or freeze, then adding 2×2 or 3×3 shapes and finally finishing with some random variations.

That's why Powerrules was born. It enables anyone to make rules thanks to a simpler usage. Its goal is to make rules accessible to anyone instead of the "more developpers" ones.

It follows a linear structure like the original DDNet rules format. The patterns have been implemented into commands to improve both the readability and the ease.

## Contributing

Contributions are welcome! Whether it concerns the backend, the frontend, the wiki or the visual design, contributions are much appreciated. There is many ways to help this project so if you are motivated, feel free to contact @lomination on Discord.

Moreover, if you have any suggestions, feel free to reach out via GitHub issues or Discord.

## Usefull Links

### Related documentation

- [DDNet official wiki](https://wiki.ddnet.org/wiki/Automapper)
- [Forum post (possibly partially outdated)](https://forum.ddnet.org/viewtopic.php?t=2428)
- [Forum post (possibly partially outdated)](https://www.teeworlds.com/forum/viewtopic.php?pid=92492)

### Similar projects

- [Simple DDNet Automapper App](https://github.com/AssassinTee/SimpleDDNetAutomapper)
- [Teeworlds Automap Tool](https://github.com/ZonsaC/Teeworlds-Automapper)
- [Teeworlds Rule Generator](https://github.com/tw-tooling/tw-rule-generator)

## Technical Information

This project is cross-compiled for both ScalaJVM and ScalaJS. The shared core is located in the `shared/` directory, while platform-specific code resides in the `js/` or `jvm/` directories.

The compilation process follows several steps. All referenced classes are found in the `shared/src/main/scala/lomination/powerrules` directory:
1. The configuration is parsed using [`config.ConfigParser`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/config/ConfigParser.scala).
2. The macro section is tokenized with [`lexing.Lexer`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/lexing/Lexer.scala).
3. Tokens are formatted using [`formatting.Formatter`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/formatting/Formatter.scala) to ease parsing.
4. The formatted tokens are parsed by [`macros.MacroParser`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/macros/MacroParser.scala).
5. The rules section is tokenized using [`lexing.Lexer`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/lexing/Lexer.scala).
6. Parsed macros are applied to the tokens in the rules section using [`macros.MacroApplier`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/macros/MacroApplier.scala).
7. The resulting tokens are formatted by [`formatting.Formatter`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/formatting/Formatter.scala).
8. Tokens are parsed using [`parsing.MainParser`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/parsing/MainParser.scala).
9. Finally, the parsed AST is written using the [`writing.Writer`](https://github.com/lomination/Powerrules/blob/main/shared/src/main/scala/lomination/powerrules/writing/Writer.scala) type class.

### ScalaJVM

The ScalaJVM part of the project allows you to convert a Powerrules file into a DDNet rules file. To use it, run the following command replacing `<filename>` by the name of your Powerrules file:

```bash
sbt "rootJVM/run <filename>"
```

It will create a new `.rules` file in the current directory, containing the compiled rules.

### ScalaJS

The ScalaJS component enables building a web-based Powerrules to DDNet rules converter. You can try it [here](https://lomination.github.io/Powerrules/) or build
