package loxx

import (
	"fmt"
	"os"
	"strconv"
)

type precedence int

const (
	precNone       precedence = iota
	precAssignment            // =
	precTernary               // ?
	precOr                    // or
	precAnd                   // and
	precEquality              // == !=
	precComparison            // < > <= >=
	precTerm                  // + -
	precFactor                // * /
	precUnary                 // ! -
	precCall                  // . ()
	precPrimary
)

type parseFunction func(c *compiler, canAssign bool)

type parseRule struct {
	nud parseFunction
	led parseFunction
	precedence
}

var rules [tokensCount]parseRule

func init() {
	rules[tokenLeftParen] = parseRule{grouping, call, precCall}       // (
	rules[tokenMinus] = parseRule{unary, binary, precTerm}            // -
	rules[tokenPlus] = parseRule{nil, binary, precTerm}               // +
	rules[tokenSlash] = parseRule{nil, binary, precFactor}            // /
	rules[tokenStar] = parseRule{nil, binary, precFactor}             // *
	rules[tokenBang] = parseRule{unary, nil, precNone}                // !
	rules[tokenBangEqual] = parseRule{nil, binary, precEquality}      // !=
	rules[tokenEqualEqual] = parseRule{nil, binary, precEquality}     // ==
	rules[tokenGreater] = parseRule{nil, binary, precComparison}      // >
	rules[tokenGreaterEqual] = parseRule{nil, binary, precComparison} // >=
	rules[tokenLess] = parseRule{nil, binary, precComparison}         // <
	rules[tokenLessEqual] = parseRule{nil, binary, precComparison}    // <=
	rules[tokenIdentifier] = parseRule{variable, nil, precNone}       // ident
	rules[tokenString] = parseRule{string_, nil, precNone}            // "string"
	rules[tokenNumber] = parseRule{number, nil, precNone}             // 12.3
	rules[tokenAnd] = parseRule{nil, and, precAnd}                    // and
	rules[tokenFalse] = parseRule{literal, nil, precNone}             // false
	rules[tokenNil] = parseRule{literal, nil, precNone}               // nil
	rules[tokenOr] = parseRule{nil, or, precOr}                       // or
	rules[tokenTrue] = parseRule{literal, nil, precNone}              // true
	rules[tokenQuestion] = parseRule{nil, ternary, precTernary}       // ?
	rules[tokenDot] = parseRule{nil, dot, precCall}                   // .
	rules[tokenThis] = parseRule{this, nil, precNone}                 // this
	rules[tokenSuper] = parseRule{super, nil, precNone}               // super
	rules[tokenYield] = parseRule{yield, nil, precNone}               // yield
	rules[tokenResume] = parseRule{resume, nil, precNone}             // resume
	rules[tokenLeftBracket] = parseRule{array, index, precCall}       // [
	rules[tokenLeftBrace] = parseRule{map_, nil, precNone}            // {
}

func map_(c *compiler, canAssign bool) {
	var pairCount uint8 = 0
	if !c.check(tokenRightBrace) {
		for {
			c.expression()
			c.consume(tokenColon, "Expect ':' after key.")
			c.expression()
			if pairCount == 255 {
				c.error("Can't have more than 255 pairs in map literal.")
			}
			pairCount++
			if !c.match(tokenComma) {
				break
			}
			if c.check(tokenRightBrace) {
				break
			}
		}
	}
	c.consume(tokenRightBrace, "Expect '}' after map pairs.")
	c.emitBytes(opMap, pairCount)
}

func array(c *compiler, canAssign bool) {
	var elemCount uint8 = 0
	if !c.check(tokenRightBracket) {
		for {
			c.expression()
			if elemCount == 255 {
				c.error("Can't have more than 255 elements in array literal.")
			}
			elemCount++
			if !c.match(tokenComma) {
				break
			}
			if c.check(tokenRightBracket) {
				break
			}
		}
	}
	c.consume(tokenRightBracket, "Expect ']' after array elements.")
	c.emitBytes(opArray, elemCount)
}

func index(c *compiler, canAssign bool) {
	c.expression()
	c.consume(tokenRightBracket, "Expect ']' after index.")
	if canAssign && c.match(tokenEqual) {
		c.expression()
		c.emitByte(opSetIndex)
	} else {
		c.emitByte(opGetIndex)
	}
}

func yield(c *compiler, canAssign bool) {
	if c.functionType != typeFunction &&
		c.functionType != typeThread {
		c.error("Can't use 'yield' outside of a thread.")
	} else {
		c.functionType = typeThread
	}
	c.expression()
	c.emitByte(opYield)
}

func resume(c *compiler, canAssign bool) {
	c.parsePrecedence(precUnary)
	if c.match(tokenWith) {
		c.expression()
	} else {
		c.emitByte(opNil)
	}
	c.emitByte(opResume)
}

func super(c *compiler, canAssign bool) {
	if c.classCompiler == nil {
		c.error("Can't use 'super' outside of a class.")
	}
	if !c.classCompiler.hasSuperClass {
		c.error("Can't use 'super' in a class with no superclass.")
	}
	c.consume(tokenDot, "Expect '.' after 'super'.")
	c.consume(tokenIdentifier, "Expect superclass method name.")
	name := c.identifierConstant(c.previous.literal)
	c.namedVariable(stringThis, false)
	if c.match(tokenLeftParen) {
		argCount := c.argumentList()
		c.namedVariable(stringSuper, false)
		c.emitBytes(opSuperInvoke, name)
		c.emitByte(argCount)
		return
	}
	c.namedVariable(stringSuper, false)
	c.emitBytes(opGetSuper, name)
}

func this(c *compiler, canAssign bool) {
	if c.classCompiler == nil {
		c.error("Can't use 'this' outside of a class.")
		return
	}
	variable(c, false)
}

func dot(c *compiler, canAssign bool) {
	c.consume(tokenIdentifier, "Expect property name after '.'.")
	name := c.identifierConstant(c.previous.literal)
	if canAssign && c.match(tokenEqual) {
		c.expression()
		c.emitBytes(opSetProperty, name)
	} else if c.match(tokenLeftParen) {
		argCount := c.argumentList()
		c.emitBytes(opInvoke, name)
		c.emitByte(argCount)
	} else {
		c.emitBytes(opGetProperty, name)
	}
}

func ternary(c *compiler, canAssign bool) {
	thenJump := c.emitJump(opJumpIfFalse)
	c.emitByte(opPop)
	c.expression()
	elseJump := c.emitJump(opJump)
	c.consume(tokenColon, "Expect ':' after expression.")
	c.patchJump(thenJump)
	c.emitByte(opPop)
	c.expression()
	c.patchJump(elseJump)
}

func or(c *compiler, canAssign bool) {
	elseJump := c.emitJump(opJumpIfFalse)
	endJump := c.emitJump(opJump)
	c.patchJump(elseJump)
	c.emitByte(opPop)
	c.parsePrecedence(precOr)
	c.patchJump(endJump)
}

func and(c *compiler, canAssign bool) {
	endJump := c.emitJump(opJumpIfFalse)
	c.emitByte(opPop)
	c.parsePrecedence(precAnd)
	c.patchJump(endJump)
}

func binary(c *compiler, canAssign bool) {
	operatorType := c.previous.tokenType
	rule := rules[operatorType]
	c.parsePrecedence(rule.precedence + 1)
	switch operatorType {
	case tokenBangEqual:
		c.emitBytes(opEqual, opNot)
	case tokenEqualEqual:
		c.emitByte(opEqual)
	case tokenGreater:
		c.emitByte(opGreater)
	case tokenGreaterEqual:
		c.emitBytes(opLess, opNot)
	case tokenLess:
		c.emitByte(opLess)
	case tokenLessEqual:
		c.emitBytes(opGreater, opNot)
	case tokenPlus:
		c.emitByte(opAdd)
	case tokenMinus:
		c.emitByte(opSubtract)
	case tokenStar:
		c.emitByte(opMultiply)
	case tokenSlash:
		c.emitByte(opDivide)
	default:
		panic("binary: unknown operator")
	}
}

func unary(c *compiler, canAssign bool) {
	operatorType := c.previous.tokenType
	c.parsePrecedence(precUnary)
	switch operatorType {
	case tokenBang:
		c.emitByte(opNot)
	case tokenMinus:
		c.emitByte(opNegate)
	default:
		panic("unary: unknown operator")
	}
}

func literal(c *compiler, canAssign bool) {
	switch c.previous.tokenType {
	case tokenNil:
		c.emitByte(opNil)
	case tokenFalse:
		c.emitByte(opFalse)
	case tokenTrue:
		c.emitByte(opTrue)
	default:
		return
	}
}

func call(c *compiler, canAssign bool) {
	argCount := c.argumentList()
	c.emitBytes(opCall, argCount)
}

func grouping(c *compiler, canAssign bool) {
	c.expression()
	c.consume(tokenRightParen, "Expect ')' after expression.")
}

var integerBases = map[byte]int{'x': 16, 'o': 8, 'b': 2}

func number(c *compiler, canAssign bool) {
	value, err := strconv.ParseFloat(c.previous.literal, 64)
	if err != nil {
		if base, ok := integerBases[lowerChar(c.previous.literal[1])]; ok {
			uintValue, err := strconv.ParseUint(c.previous.literal[2:], base, 64)
			if err != nil {
				panic(err)
			}
			c.emitConstant(Number(uintValue))
			return
		}
		panic(err)
	}
	c.emitConstant(Number(value))
}

func string_(c *compiler, canAssign bool) {
	c.emitConstant(
		String(c.previous.literal[1 : len(c.previous.literal)-1]),
	)
}

func variable(c *compiler, canAssign bool) {
	c.namedVariable(c.previous.literal, canAssign)
}

type parser struct {
	scanner
	current   token
	previous  token
	hadError  bool
	panicMode bool
}

func newParser(source []byte) parser {
	p := parser{scanner: newScanner(source)}
	p.advance()
	return p
}

func (p *parser) errorAt(token *token, message string) {
	if p.panicMode {
		return
	}
	p.panicMode = true
	fmt.Fprintf(os.Stderr, "[line %d] Error", token.line)

	switch token.tokenType {
	case tokenEof:
		fmt.Fprintf(os.Stderr, " at end")
	case tokenError:
	default:
		fmt.Fprintf(os.Stderr, " at '%s'", token.literal)
	}

	fmt.Fprintf(os.Stderr, ": %s\n", message)
	p.hadError = true
}

func (p *parser) error(message string) {
	p.errorAt(&p.previous, message)
}

func (p *parser) errorAtCurrent(message string) {
	p.errorAt(&p.current, message)
}

func (p *parser) advance() {
	p.previous = p.current
	for {
		p.current = p.scanner.scanToken()
		if p.current.tokenType != tokenError {
			break
		}
		p.errorAtCurrent(p.current.literal)
	}
}

func (p *parser) consume(t tokenType, message string) {
	if p.current.tokenType == t {
		p.advance()
		return
	}
	p.errorAtCurrent(message)
}

func (p *parser) consumeSemi(message string) {
	if modeAutoSemicolons {
		if p.current.tokenType == tokenSemicolon {
			p.advance()
			return
		}
	} else {
		p.consume(tokenSemicolon, message)
	}
}

func (p *parser) check(t tokenType) bool {
	return p.current.tokenType == t
}

func (p *parser) match(t tokenType) bool {
	if !p.check(t) {
		return false
	}
	p.advance()
	return true
}

type localVariable struct {
	name      string
	depth     int
	isUpvalue bool
}

type compilerUpvalue struct {
	index   uint8
	isLocal bool
}

type classCompiler struct {
	hasSuperClass bool
	enclosing     *classCompiler
}

type functionType int

const (
	typeScript functionType = iota
	typeFunction
	typeThread
	typeMethod
	typeInitializer
)

type loopCompiler struct {
	start     int
	breaks    []int
	enclosing *loopCompiler
}

type compiler struct {
	*parser
	function *function
	functionType
	locals   []localVariable
	upvalues []compilerUpvalue
	*classCompiler
	*loopCompiler
	enclosing  *compiler
	scopeDepth int
}

func newCompiler(source []byte) *compiler {
	p := newParser(source)
	return &compiler{
		parser:        &p,
		function:      newFunction(),
		functionType:  typeScript,
		locals:        []localVariable{{"", 0, false}},
		upvalues:      make([]compilerUpvalue, 0),
		classCompiler: nil,
		loopCompiler:  nil,
		enclosing:     nil,
		scopeDepth:    0,
	}
}

func (c *compiler) newFunCompiler(t functionType) compiler {
	f := newFunction()
	var locals []localVariable
	if t == typeMethod || t == typeInitializer {
		locals = []localVariable{{stringThis, 0, false}}
	} else {
		locals = []localVariable{{"", 0, false}}
	}
	f.name = String(c.previous.literal)
	return compiler{
		parser:        c.parser,
		function:      f,
		functionType:  t,
		locals:        locals,
		upvalues:      make([]compilerUpvalue, 0),
		classCompiler: c.classCompiler,
		loopCompiler:  nil,
		enclosing:     c,
		scopeDepth:    0,
	}
}

func (c *compiler) compile() *function {
	for !c.match(tokenEof) {
		c.declaration()
	}
	if c.hadError {
		return nil
	}
	c.emitReturn()
	return c.function
}

func (c *compiler) declaration() {
	if c.match(tokenFun) {
		c.funDeclaration()
	} else if c.match(tokenVar) {
		c.varDeclaration()
	} else if c.match(tokenClass) {
		c.classDeclaration()
	} else if c.match(tokenImport) {
		c.importDeclaration()
	} else {
		c.statement()
	}

	if c.panicMode {
		c.synchronize()
	}
}

func (c *compiler) statement() {
	if c.match(tokenSemicolon) {
		/* pass */
	} else if c.match(tokenPrint) {
		c.printStatement()
	} else if c.match(tokenFor) {
		c.forStatement()
	} else if c.match(tokenIf) {
		c.ifStatement()
	} else if c.match(tokenReturn) {
		c.returnStatement()
	} else if c.match(tokenWhile) {
		c.whileStatement()
	} else if c.match(tokenLeftBrace) {
		c.beginScope()
		c.block()
		c.endScope()
	} else if c.match(tokenBreak) {
		c.breakStatement()
	} else if c.match(tokenContinue) {
		c.continueStatement()
	} else {
		c.expressionStatement()
	}
}

// declarations ============================================================== /

func (c *compiler) funDeclaration() {
	global := c.parseVariable("Expect function name.")
	c.markInitialized()
	c.compileFunction(typeFunction)
	c.defineVariable(global)
}

func (c *compiler) classDeclaration() {
	c.consume(tokenIdentifier, "Expect class name.")
	nameConstant := c.identifierConstant(c.previous.literal)
	className := c.previous.literal
	c.declareVariable()

	c.emitBytes(opClass, nameConstant)
	c.defineVariable(nameConstant)

	c.classCompiler = &classCompiler{false, c.classCompiler}

	if c.match(tokenLess) {
		c.consume(tokenIdentifier, "Expect superclass name.")
		if className == c.previous.literal {
			c.error("A class can't inherit from itself.")
		}
		variable(c, false)

		c.beginScope()
		c.addLocal(stringSuper)
		c.defineVariable(0)

		c.namedVariable(className, false)
		c.emitByte(opInherit)
		c.classCompiler.hasSuperClass = true
	}

	c.namedVariable(className, false)

	c.consume(tokenLeftBrace, "Expect '{' before class body.")
	for !c.check(tokenRightBrace) && !c.check(tokenEof) {
		c.method()
		for c.check(tokenSemicolon) {
			c.advance()
		}
	}
	c.consume(tokenRightBrace, "Expect '}' after class body.")

	c.emitByte(opPop)

	if c.classCompiler.hasSuperClass {
		c.endScope()
	}

	c.classCompiler = c.classCompiler.enclosing
}

func (c *compiler) importDeclaration() {
	if c.functionType != typeScript {
		c.error("Can't import to non-top level scope.")
	}
	name := c.parseVariable("Expect module name.")
	c.markInitialized()
	c.consumeSemi("Expect ';' after module name.")
	c.emitBytes(opImport, name)
}

func (c *compiler) varDeclaration() {
	global := c.parseVariable("Expect variable name.")
	if c.match(tokenEqual) {
		c.expression()
	} else {
		c.emitByte(opNil)
	}
	c.consumeSemi("Expect ';' after variable declaration.")
	c.defineVariable(global)
}

// statements ================================================================ /

func (c *compiler) block() {
	for !c.check(tokenRightBrace) && !c.check(tokenEof) {
		c.declaration()
	}
	c.consume(tokenRightBrace, "Expect '}' after block.")
}

func (c *compiler) ifStatement() {
	c.consume(tokenLeftParen, "Expect '(' after 'if'.")
	c.expression()
	c.consume(tokenRightParen, "Expect ')' after condition.")

	thenJump := c.emitJump(opJumpIfFalse)

	c.emitByte(opPop)

	c.statement()

	elseJump := c.emitJump(opJump)

	c.patchJump(thenJump)

	c.emitByte(opPop)

	if c.match(tokenElse) {
		c.statement()
	}

	c.patchJump(elseJump)
}

func (c *compiler) printStatement() {
	c.expression()
	c.consumeSemi("Expect ';' after value.")
	c.emitByte(opPrint)
}

func (c *compiler) returnStatement() {
	if c.functionType == typeScript {
		c.error("Can't return from top-level code.")
	}

	if c.match(tokenSemicolon) {
		c.emitReturn()
	} else {
		if c.functionType == typeInitializer {
			c.error("Can't return a value from an initializer.")
		}
		c.expression()
		c.consumeSemi("Expect ';' after return value.")
		c.emitByte(opReturn)
	}
}

func (c *compiler) forStatement() {
	c.beginScope()
	c.consume(tokenLeftParen, "Expect '(' after 'for'.")
	if c.match(tokenSemicolon) {

	} else if c.match(tokenVar) {
		c.varDeclaration()
	} else {
		c.expressionStatement()
	}

	c.beginLoop()
	loopStart := len(c.function.chunk.code)
	exitJump := -1
	if !c.match(tokenSemicolon) {
		c.expression()
		c.consumeSemi("Expect ';' after loop condition.")

		exitJump = c.emitJump(opJumpIfFalse)
		c.emitByte(opPop)
	}

	if !c.match(tokenRightParen) {
		bodyJump := c.emitJump(opJump)
		incrementStart := len(c.function.chunk.code)
		c.expression()
		c.emitByte(opPop)
		c.consume(tokenRightParen, "Expect ')' after for clauses.")

		c.emitLoop(loopStart)
		c.loopCompiler.start = incrementStart
		loopStart = incrementStart
		c.patchJump(bodyJump)
	}

	c.statement()
	c.emitLoop(loopStart)

	if exitJump != -1 {
		c.patchJump(exitJump)
		c.emitByte(opPop)
	}

	c.endLoop()
	c.endScope()
}

func (c *compiler) whileStatement() {
	c.beginLoop()
	loopStart := len(c.function.chunk.code)

	c.consume(tokenLeftParen, "Expect '(' after 'while'.")
	c.expression()
	c.consume(tokenRightParen, "Expect ')' after condition.")

	exitJump := c.emitJump(opJumpIfFalse)
	c.emitByte(opPop)
	c.statement()

	c.emitLoop(loopStart)

	c.patchJump(exitJump)
	c.emitByte(opPop)

	c.endLoop()
}

func (c *compiler) breakStatement() {
	if c.loopCompiler == nil {
		c.error("Can't use 'break' outside of a loop.")
		return
	}
	c.consumeSemi("Expect ';' after 'break'.")
	c.loopCompiler.breaks = append(c.loopCompiler.breaks, c.emitJump(opJump))
}

func (c *compiler) continueStatement() {
	if c.loopCompiler == nil {
		c.error("Can't use 'continue' outside of a loop.")
		return
	}
	c.consumeSemi("Expect ';' after 'continue'.")
	c.emitLoop(c.loopCompiler.start)
}

func (c *compiler) expressionStatement() {
	c.expression()
	c.consumeSemi("Expect ';' after expression.")
	c.emitByte(opPop)
}

// other ===================================================================== /

func (c *compiler) expression() {
	c.parsePrecedence(precAssignment)
}

func (c *compiler) parsePrecedence(prec precedence) {
	c.advance()
	prefixRule := rules[c.previous.tokenType].nud
	if prefixRule == nil {
		c.error("Expect expression.")
		return
	}

	canAssign := prec <= precAssignment
	prefixRule(c, canAssign)

	for prec <= rules[c.current.tokenType].precedence {
		c.advance()
		infixRule := rules[c.previous.tokenType].led
		infixRule(c, canAssign)
	}

	if canAssign && c.match(tokenEqual) {
		c.error("Invalid assignment target.")
	}
}

func (c *compiler) beginScope() { c.scopeDepth++ }

func (c *compiler) endScope() {
	c.scopeDepth--

	for len(c.locals) > 0 && c.locals[len(c.locals)-1].depth > c.scopeDepth {
		if c.locals[len(c.locals)-1].isUpvalue {
			c.emitByte(opCloseUpvalue)
		} else {
			c.emitByte(opPop)
		}
		c.locals = c.locals[:len(c.locals)-1]
	}
}

func (c *compiler) parseVariable(message string) uint8 {
	c.consume(tokenIdentifier, message)
	c.declareVariable()
	if c.scopeDepth > 0 {
		return 0
	}
	return c.identifierConstant(c.previous.literal)
}

func (c *compiler) identifierConstant(name string) uint8 {
	return c.makeConstant(String(name))
}

func (c *compiler) markInitialized() {
	if c.scopeDepth == 0 {
		return
	}
	c.locals[len(c.locals)-1].depth = c.scopeDepth
}

func (c *compiler) defineVariable(global uint8) {
	if c.scopeDepth > 0 {
		c.markInitialized()
		return
	}
	c.emitBytes(opDefineGlobal, global)
}

func (c *compiler) declareVariable() {
	if c.scopeDepth == 0 {
		return
	}
	name := c.previous.literal
	for i := len(c.locals) - 1; i >= 0; i-- {
		local := &c.locals[i]
		if local.depth < c.scopeDepth && local.depth != -1 {
			break
		}
		if local.name == name {
			c.error("Already a variable with this name in this scope.")
		}
	}
	c.addLocal(name)
}

func (c *compiler) addLocal(name string) {
	if len(c.locals) == uint8Count {
		c.error("Too many local variables in function.")
	}
	c.locals = append(c.locals, localVariable{name, -1, false})
}

func (c *compiler) compileFunction(t functionType) {
	fc := c.newFunCompiler(t)
	fc.beginScope()

	fc.consume(tokenLeftParen, "Expect '(' after function name.")

	if !fc.check(tokenRightParen) {
		for {
			fc.function.arity++
			if fc.function.arity > 255 {
				fc.errorAtCurrent("Can't have more than 255 parameters.")
			}
			constant := fc.parseVariable("Expect parameter name.")
			fc.defineVariable(constant)
			if !fc.match(tokenComma) {
				break
			}
			if c.check(tokenRightParen) {
				break
			}
		}
	}
	fc.consume(tokenRightParen, "Expect ')' after parameters.")
	fc.consume(tokenLeftBrace, "Expect '{' before function body.")
	fc.block()
	fc.emitReturn()

	c.emitBytes(opClosure, c.makeConstant(fc.function))

	for i := 0; i < fc.function.upvalueCount; i++ {
		c.emitByte(boolToUint8(fc.upvalues[i].isLocal))
		c.emitByte(fc.upvalues[i].index)
	}

	if fc.functionType == typeThread {
		c.emitByte(opThread)
	}
}

func (c *compiler) method() {
	c.consume(tokenIdentifier, "Expect method name.")
	constant := c.identifierConstant(c.previous.literal)
	t := typeMethod
	if c.previous.literal == stringInit {
		t = typeInitializer
	}
	c.compileFunction(t)
	c.emitBytes(opMethod, constant)
}

func (c *compiler) beginLoop() {
	c.loopCompiler = &loopCompiler{len(c.function.chunk.code), nil, c.loopCompiler}
}

func (c *compiler) endLoop() {
	for _, breakJump := range c.loopCompiler.breaks {
		c.patchJump(breakJump)
	}
	c.loopCompiler = c.loopCompiler.enclosing
}

func (c *compiler) emitByte(b uint8) {
	c.function.chunk.writeCode(b, c.previous.line)
}

func (c *compiler) emitBytes(b1, b2 uint8) {
	c.emitByte(b1)
	c.emitByte(b2)
}

func (c *compiler) emitLoop(loopStart int) {
	c.emitByte(opLoop)

	offset := len(c.function.chunk.code) - loopStart + 2
	if offset > int(uint16Max) {
		c.error("Loop body too large.")
	}

	c.emitByte(uint8((offset >> 8) & 0xff))
	c.emitByte(uint8(offset & 0xff))
}

func (c *compiler) emitJump(instruction uint8) int {
	c.emitByte(instruction)
	c.emitByte(0xff)
	c.emitByte(0xff)
	return len(c.function.chunk.code) - 2
}

func (c *compiler) emitReturn() {
	if c.functionType == typeInitializer {
		c.emitBytes(opGetLocal, 0)
	} else {
		c.emitByte(opNil)
	}
	c.emitByte(opReturn)
}

func (c *compiler) makeConstant(value Value) uint8 {
	constant := c.function.chunk.addConstant(value)
	if constant > int(uint8Max) {
		c.error("Too many constants in one chunk.")
		return 0
	}
	return uint8(constant)
}

func (c *compiler) emitConstant(value Value) {
	c.emitBytes(opConstant, c.makeConstant(value))
}

func (c *compiler) patchJump(offset int) {
	jump := len(c.function.chunk.code) - offset - 2

	if jump > int(uint16Max) {
		c.error("Too much code to jump over.")
	}

	c.function.chunk.code[offset] = uint8((jump >> 8) & 0xff)
	c.function.chunk.code[offset+1] = uint8(jump & 0xff)
}

func (c *compiler) namedVariable(name string, canAssign bool) {
	var getOp, setOp uint8

	var arg int
	if arg = c.resolveLocal(name); arg != -1 {
		getOp = opGetLocal
		setOp = opSetLocal
	} else if arg = c.resolveUpvalue(name); arg != -1 {
		getOp = opGetUpvalue
		setOp = opSetUpvalue
	} else {
		arg = int(c.identifierConstant(name))
		getOp = opGetGlobal
		setOp = opSetGlobal
	}

	if canAssign && c.match(tokenEqual) {
		c.expression()
		c.emitBytes(setOp, uint8(arg))
	} else {
		c.emitBytes(getOp, uint8(arg))
	}
}

func (c *compiler) resolveUpvalue(name string) int {
	if c.enclosing == nil {
		return -1
	}
	if local := c.enclosing.resolveLocal(name); local != -1 {
		c.enclosing.locals[local].isUpvalue = true
		return c.addUpvalue(local, true)
	}
	if upval := c.enclosing.resolveUpvalue(name); upval != -1 {
		return c.addUpvalue(upval, false)
	}
	return -1
}

func (c *compiler) addUpvalue(index int, isLocal bool) int {
	for i := len(c.upvalues) - 1; i >= 0; i-- {
		if c.upvalues[i].index == uint8(index) && c.upvalues[i].isLocal == isLocal {
			return i
		}
	}

	if len(c.upvalues) == uint8Count {
		c.error("Too many closure variables in function.")
	}

	c.upvalues = append(c.upvalues, compilerUpvalue{uint8(index), isLocal})
	c.function.upvalueCount++
	return len(c.upvalues) - 1
}

func (c *compiler) resolveLocal(name string) int {
	for i := len(c.locals) - 1; i >= 0; i-- {
		if c.locals[i].name == name {
			if c.locals[i].depth == -1 {
				c.error("Can't read local variable in its own initializer.")
			}
			return i
		}
	}
	return -1
}

func (c *compiler) argumentList() uint8 {
	var argCount uint8 = 0
	if !c.check(tokenRightParen) {
		for {
			c.expression()
			if argCount == 255 {
				c.error("Can't have more than 255 arguments.")
			}
			argCount++
			if !c.match(tokenComma) {
				break
			}
			if c.check(tokenRightParen) {
				break
			}
		}
	}
	c.consume(tokenRightParen, "Expect ')' after arguments.")
	return argCount
}

func (c *compiler) synchronize() {
	c.panicMode = false

	for c.current.tokenType != tokenEof {
		if c.previous.tokenType == tokenSemicolon {
			return
		}
		switch c.current.tokenType {
		case tokenVar, tokenFun, tokenFor, tokenIf,
			tokenWhile, tokenPrint, tokenReturn:
			return
		}

		c.advance()
	}
}
