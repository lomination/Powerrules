[![Build](https://github.com/lomination/Powerrules/actions/workflows/build.yaml/badge.svg)](https://github.com/lomination/Powerrules/actions/workflows/build.yaml)

## Powerrules

Powerrules is a language designed to generate rules for [DDNet](https://ddnet.org) ([GitHub](https://github.com/ddnet/ddnet)). Its goal is to simplify writing rules by providing a more user-friendly and powerful syntax.

You can use the [online transpiler](https://lomination.github.io/Powerrules/) to convert Powerrules into DDNet rules. For more information on how to write Powerrules, check out the [wiki](https://github.com/lomination/Powerrules/wiki).

Special thanks to Toom for significant technical help and advice with Scala, Ssor for support and ideas, and archimede67 for fixing bugs that almost led me to abandon the project!

## Project Structure

This project is cross-compiled for both ScalaJVM and ScalaJS. The shared core is located in the `shared/` directory, while platform-specific code resides in the `js/` or `jvm/` directories.

The compilation process follows several steps. All referenced classes are found in the `shared/src/main/scala/lomination/powerrules` directory:
1. The configuration is parsed using `config.ConfigParser`.
2. The macro section is tokenized with `lexing.Lexer`.
3. Tokens are formatted using `formatting.Format` to ease parsing.
4. The formatted tokens are parsed by `macros.MacroParser`.
5. The rules section is tokenized using `lexing.Lexer`.
6. Parsed macros are applied to the tokens in the rules section using `macros.MacroApplier`.
7. The resulting tokens are formatted by `formatting.Formatter`.
8. Tokens are parsed using `powerrulesparsing.PowerrulesParser`.
9. Finally, the parsed AST is written using the `writing.Writer` type class.

### ScalaJVM

The ScalaJVM part of the project allows you to convert a Powerrules file into a DDNet rules file. To use it, modify `example.txt` (input file) and `example.rules` (output file) in `jvm/src/main/scala/lomination/powerrules/Main.scala`. Then, run the following command:

```bash
sbt "rootJVM/run <filename>"
```

### ScalaJS

The ScalaJS component enables building a web-based Powerrules to DDNet rules converter. You can try it [here](https://lomination.github.io/Powerrules/) or build it yourself with:

```bash
npm run build
```

## Contributing

Contributions are welcome, whether in code (especially for the frontend) or the wiki. If you have any suggestions, feel free to reach out via GitHub issues or Discord.
