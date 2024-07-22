import std/[strutils, sequtils, tables, options, algorithm]

type
  TokenKind* = enum
    tkText, tkCommand, tkBegin, tkEnd, tkBracketOpen, tkBracketClose, tkCurlyOpen, tkCurlyClose

  TexNodeKind* = enum
    txText, txCommand, txOption, txOptList, txArgument, txArgList, txEnvironment, txEnvironmentEnd

  TexObj* = object
    case kind*: TexNodeKind
    of txText:
      text*: string
    of txCommand:
      name*: string
      arguments*: TexNode # `txArgList`
      optionalArguments*: TexNode # `txOptList`
    of txArgument:
      arg*: seq[TexNode] # can be anything
    of txArgList:
      args*: seq[TexNode] # all `txArgument`
    of txOption:
      opt*: seq[TexNode] # can be anything
    of txOptList:
      opts*: seq[TexNode] # all `txOption`
    of txEnvironment:
      envName*: string
      optArgs*: TexNode # txOption
      content*: seq[TexNode]
    of txEnvironmentEnd: ## only for saner parsing, won't appear in final `seq[TexNode]` "AST"
      envNameEnd*: string
  TexNode* = ref TexObj

  Token* = object
    kind*: TokenKind
    value*: string

proc `$`*(e: TexNode): string
proc stringify*(e: TexObj): string =
  case e.kind
  of txText: result = e.text
  of txCommand:
    result = "\\" & e.name
    result.add $e.optionalArguments # if nil, just be empty
    result.add $e.arguments
  of txOption:
    if e.opt.len > 0:
      result = "["
      for ch in e.opt:
        result.add $ch
      result.add "]"
  of txOptList:
    for ch in e.opts:
      result.add $ch
  of txArgument:
    if e.arg.len > 0:
      result = "{"
      for ch in e.arg:
        result.add $ch
      result.add "}"
  of txArgList:
    for ch in e.args:
      result.add $ch
  of txEnvironment:
    result.add "\\begin{" & $e.envName & "}"
    result.add $e.optArgs # if nil, just empy
    for ch in e.content:
      result.add $ch
    result.add "\\end{" & $e.envName & "}"
  else:
    discard

proc `$`*(e: TexObj): string = e.stringify()

proc `$`*(e: TexNode): string =
  if e != nil:
    result = stringify(e[])

proc `$`*(doc: seq[TexNode]): string =
  for ch in doc:
    result.add $ch

proc tokenize*(latex: string): seq[Token] =
  var tokens: seq[Token]
  var i = 0
  while i < latex.len:
    if latex[i] == '\\':
      var j = i + 1
      while j < latex.len and latex[j].isAlphaNumeric:
        inc j
      if j < latex.len and latex[j] == '*': # last can be `*`
        inc j
      let name = latex[i ..< j]
      case name
      of "\\begin": tokens.add(Token(kind: tkBegin, value: name))
      of "\\end":   tokens.add(Token(kind: tkEnd, value: name))
      else:       tokens.add(Token(kind: tkCommand, value: name))
      i = j
    elif latex[i] == '{': tokens.add Token(kind: tkCurlyOpen); inc i
    elif latex[i] == '}': tokens.add Token(kind: tkCurlyClose); inc i
    elif latex[i] == '[': tokens.add Token(kind: tkBracketOpen); inc i
    elif latex[i] == ']': tokens.add Token(kind: tkBracketClose); inc i
    else:
      var j = i
      while j < latex.len and latex[j] notin {'\\', '{', '}', '[', ']'}:
        inc j
      tokens.add(Token(kind: tkText, value: latex[i..<j]))
      i = j
  tokens

proc parseToken*(token: Token, tokens: var seq[Token]): TexNode

proc parseArgument*(token: Token, tokens: var seq[Token]): TexNode =
  doAssert token.kind == tkCurlyOpen, "Is instead: " & $token
  doAssert tokens.len > 0, "Cannot be empty!"
  var args: seq[TexNode]
  while tokens.len > 0: ## Will break when `tkCurlyClose` encountered
    let t = tokens.pop()
    case t.kind
    of tkCurlyOpen: args.add parseArgument(t, tokens)
    of tkCurlyClose: break # found the end of _this_ token, because we recursed for all internal
    else: args.add parseToken(t, tokens)
  result = TexNode(kind: txArgument, arg: args)

proc parseOptionalArgument*(token: Token, tokens: var seq[Token]): TexNode =
  doAssert token.kind == tkBracketOpen, "Is instead: " & $token
  doAssert tokens.len > 0, "Cannot be empty!"
  var args: seq[TexNode]
  while tokens.len > 0: ## Will break when `tkBracketClose` encountered
    let t = tokens.pop()
    case t.kind
    of tkBracketOpen: args.add parseOptionalArgument(t, tokens)
    of tkBracketClose: break
    else: args.add parseToken(t, tokens)
  result = TexNode(kind: txOption, opt: args)

proc parseEnvironment*(token: Token, tokens: var seq[Token]): TexNode =
  doAssert token.kind == tkBegin, "Is instead: " & $token
  doAssert tokens.len > 0, "Cannot be empty!"
  var content: seq[TexNode]
  var optArgs: TexNode
  var seenOther = false # true if seen anything other than empty space after beginning of environment
  var name: string
  while tokens.len > 0: ## Will break when `tkCurlyClose` encountered
    let t = tokens.pop()
    case t.kind
    of tkBegin: content.add parseEnvironment(t, tokens)
    of tkEnd: break # found the end of _this_ environment, because we recursed for all internal
    else:
      let tx = parseToken(t, tokens)
      if not seenOther and tx.kind == txOption:
        optArgs = tx
        seenOther = true
      elif not seenOther and tx.kind == txArgument and name.len == 0:
        doAssert tx.arg.len == 1, "The `\\begin` environment must be a simple name, but is: " & $tx
        doAssert tx.arg[0].kind == txText, "The `\\begin` environment argument is not raw text, but is: " & $tx
        name = tx.arg[0].text
      else:
        if not (tx.kind == txText and tx.text.strip.len > 0):
          seenOther = true #
        content.add tx
  result = TexNode(kind: txEnvironment, envName: name, optArgs: optArgs, content: content)

proc parseArguments*(tokens: var seq[Token]): TexNode =
  ## Important that this proc is called only for places where arguments are expected,
  ## i.e. after `txCommand`!
  var args: seq[TexNode]
  while tokens.len > 0 and tokens[^1].kind == tkCurlyOpen:
    let t = tokens.pop()
    args.add parseArgument(t, tokens)
  result = TexNode(kind: txArgList, args: args)

proc parseOptionalArguments*(tokens: var seq[Token]): TexNode =
  ## Important that this proc is called only for places where arguments are expected,
  ## i.e. after `txCommand`!
  var args: seq[TexNode]
  while tokens.len > 0 and tokens[^1].kind == tkBracketOpen:
    let t = tokens.pop()
    args.add parseOptionalArgument(t, tokens)
  result = TexNode(kind: txOptList, opts: args)

proc parseToken*(token: Token, tokens: var seq[Token]): TexNode =
  case token.kind
  of tkText:
    result = TexNode(kind: txText, text: token.value)
  of tkCurlyOpen:
    # parse argument
    result = parseArgument(token, tokens)
  of tkBracketOpen:
    # parse optional argument
    result = parseOptionalArgument(token, tokens)
  of tkBegin:
    # parse environment
    result = parseEnvironment(token, tokens)
  of tkCommand:
    let name = token.value.strip(chars = {'\\'}, trailing = false)
    let optArgs = parseOptionalArguments(tokens)
    let args = parseArguments(tokens)
    result = TexNode(kind: txCommand, name: name, arguments: args, optionalArguments: optArgs)
  of tkCurlyClose, tkBracketClose, tkEnd:
    discard  # This should not happen in the initial token stream

proc parse*(tokens: var seq[Token]): seq[TexNode] =
  ## `Tokens` is in reverse order. That way we can just `pop`.
  while tokens.len > 0:
    let t = tokens.pop()
    result.add t.parseToken(tokens)

proc parseTex*(s: string): seq[TexNode] =
  var toks = tokenize(s).reversed
  result = parse(toks)
