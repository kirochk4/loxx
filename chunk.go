package loxx

const (
	opConstant uint8 = iota
	opNil
	opTrue
	opFalse
	opPop
	opGetLocal
	opSetLocal
	opGetGlobal
	opDefineGlobal
	opSetGlobal
	opGetUpvalue
	opSetUpvalue
	opGetProperty
	opSetProperty
	opImport
	opClass
	opMethod
	opInherit
	opGetSuper
	opEqual
	opGreater
	opLess
	opAdd
	opSubtract
	opMultiply
	opDivide
	opNot
	opNegate
	opPrint
	opJump
	opJumpIfFalse
	opLoop
	opCall
	opClosure
	opCloseUpvalue
	opReturn
	opInvoke
	opSuperInvoke
	opThread
	opYield
	opResume
	opArray
	opSetIndex
	opGetIndex
	opMap
)

type chunk struct {
	code      []uint8
	lines     []int
	constants []Value
}

func newChunk() chunk {
	return chunk{
		code:      make([]uint8, 0),
		lines:     make([]int, 0),
		constants: make([]Value, 0),
	}
}

func (c *chunk) writeCode(code uint8, line int) {
	c.code = append(c.code, code)
	c.lines = append(c.lines, line)
}

func (c *chunk) addConstant(constant Value) int {
	for i, declared := range c.constants {
		if declared == constant {
			return i
		}
	}
	c.constants = append(c.constants, constant)
	return len(c.constants) - 1
}
