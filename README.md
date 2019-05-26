# Zlang
A small compiler, created in Rust

Improved reimplementation of [https://github.com/Zykai/Compiler](https://github.com/Zykai/Compiler)

## Progress
- [x] Lexer
- [x] Parser
- [ ] Semantic Analyzer
- [ ] Byte-Code Generator
- [ ] Virtual Machine
- [ ] Interpreter
- [ ] Webserver/-interface

## Usage (once finished)
The compiler can be run as a simple command line tool:
```
zlang.exe main.z
```
Additionally, you can run a webserver which compiles and executes code
```
zlang.exe --webserver
```
The webserver also includes a simple code editor ([Monaco](https://github.com/microsoft/monaco-editor))
```
zlang.exe --webserver --webeditor
```
