[![Build](https://github.com/lomination/Powerrules/actions/workflows/build.yaml/badge.svg)](https://github.com/lomination/Powerrules/actions/workflows/build.yaml)

## Powerrules

The Powerrules is a language that can generate DDNet rules language and aims to make it simpler to write rules by offering a new, easier and more powerful language.

You can use the [online transpiler](https://lomination.github.io/Powerrules/) to convert Powerrules into DDNet rules. See the [wiki](https://github.com/lomination/Powerrules/wiki) to learn more about writing Powerrules.

Special thanks to Toom for the huge technical help and advices with Scala, to Ssor for support and ideas, and to archimede67 for fxing bugs that almost make me give up the project

## Project

This project is cross-compiled with ScalaJVM and ScalaJS. The common core of the project is located in the `shared/` directory. The remainder is either in `js/` or `jvm/` directories.

### ScalaJVM project

The ScalaJVM part of the project allows you to convert a Powerrules file in DDNet rules file. To do, replace `example.txt` (input file's name) and `example.rules` (output file's name) by the desired files in `jvm/src/main/scala/lomination/powerrules/Main.scala`. Then you can run it using:

```
sbt rootJVM/run
```

### ScalaJS project

The ScalaJS part can build an automatic Powerrules to DDNet rules web converter. You can try it [here](https://lomination.github.io/Powerrules/) or build it yourseft using:

```
npm run build
```

## Contribution

Contributions are welcome, whether they concern the code (especially the frontend) or the wiki. If you have any suggestions, please let me know via the github issues or discord.
