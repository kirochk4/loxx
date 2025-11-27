package loxx

import "fmt"

func disassembleFunction(f *function) {
	var name String = "::main::"
	if f.name != "" {
		name = f.name
	}
	disassembleChunk(&f.chunk, name)
	for _, c := range f.chunk.constants {
		if f, ok := c.(*function); ok {
			disassembleFunction(f)
		}
	}
}

func disassembleChunk(chunk *chunk, name String) {
	fmt.Println(cover(string(name), 16, "="))

	for offset := 0; offset < len(chunk.code); {
		offset = disassembleInstruction(chunk, offset)
		fmt.Println()
	}
}

func disassembleInstruction(chunk *chunk, offset int) int {
	fmt.Printf("%04d", offset)
	if offset > 0 && chunk.lines[offset] == chunk.lines[offset-1] {
		fmt.Printf("   | ")
	} else {
		fmt.Printf("%4d ", chunk.lines[offset])
	}

	switch instruction := chunk.code[offset]; instruction {
	case opNil, opFalse, opTrue, opPop, opEqual, opGreater, opLess,
		opAdd, opSubtract, opMultiply, opDivide, opNot, opNegate, opPrint,
		opReturn, opInherit, opThread, opCloseUpvalue, opResume, opYield,
		opSetIndex, opGetIndex:
		return simpleInstruction(chunk, offset)
	case opConstant, opGetGlobal, opDefineGlobal, opSetGlobal, opClass,
		opSetProperty, opGetProperty, opMethod, opImport, opGetSuper:
		return constantInstruction(chunk, offset)
	case opGetLocal, opSetLocal, opGetUpvalue, opSetUpvalue, opCall,
		opArray, opMap:
		return byteInstruction(chunk, offset)
	case opJump, opJumpIfFalse, opLoop:
		var sign int = 1
		if instruction == opLoop {
			sign = -1
		}
		return jumpInstruction(chunk, offset, sign)
	case opInvoke, opSuperInvoke:
		return invokeInstruction(chunk, offset)
	case opClosure:
		name := instructionNames[chunk.code[offset]]
		offset++
		constant := chunk.code[offset]
		offset++
		fmt.Printf("%-16s |> %04d ", name, constant)
		fmt.Print(sprintValue(chunk.constants[constant]))
		function := chunk.constants[constant].(*function)
		for i := range function.upvalueCount {
			isLocal := chunk.code[offset+i*2]
			index := chunk.code[offset+i*2+1]
			var status string
			if isLocal == 1 {
				status = "local"
			} else {
				status = "upvalue"
			}
			fmt.Printf("\n%04d   |                  |> %s %d", offset-2, status, index)
		}
		return offset + (2 * function.upvalueCount)
	default:
		panic("disassemble instruction: unknown instruction")
	}
}

func constantInstruction(chunk *chunk, offset int) int {
	name := instructionNames[chunk.code[offset]]
	constant := chunk.code[offset+1]
	fmt.Printf("%-16s |> %04d '%s'", name, constant, sprintValue(chunk.constants[constant]))

	return offset + 2
}

func simpleInstruction(chunk *chunk, offset int) int {
	name := instructionNames[chunk.code[offset]]
	fmt.Printf("%-16s |", name)
	return offset + 1
}

func byteInstruction(chunk *chunk, offset int) int {
	name := instructionNames[chunk.code[offset]]
	slot := chunk.code[offset+1]
	fmt.Printf("%-16s |> %04d", name, slot)
	return offset + 2
}

func jumpInstruction(chunk *chunk, offset int, sign int) int {
	name := instructionNames[chunk.code[offset]]
	jump := uint16(chunk.code[offset+1]) << 8
	jump |= uint16(chunk.code[offset+2])
	fmt.Printf("%-16s |> %04d -> %04d", name, offset, offset+3+sign*int(jump))
	return offset + 3
}

func invokeInstruction(chunk *chunk, offset int) int {
	name := instructionNames[chunk.code[offset]]
	constant := chunk.code[offset+1]
	argCount := chunk.code[offset+2]
	fmt.Printf("%-16s |> (%04d args) %04d '%s'", name, argCount, constant, sprintValue(chunk.constants[constant]))
	return offset + 3
}

var instructionNames = [...]string{
	opConstant:     "OP_CONSTANT",
	opNil:          "OP_NIL",
	opTrue:         "OP_TRUE",
	opFalse:        "OP_FALSE",
	opPop:          "OP_POP",
	opGetLocal:     "OP_GET_LOCAL",
	opSetLocal:     "OP_SET_LOCAL",
	opGetGlobal:    "OP_GET_GLOBAL",
	opDefineGlobal: "OP_DEFINE_GLOBAL",
	opSetGlobal:    "OP_SET_GLOBAL",
	opGetUpvalue:   "OP_GET_UPVALUE",
	opSetUpvalue:   "OP_SET_UPVALUE",
	opEqual:        "OP_EQUAL",
	opGreater:      "OP_GREATER",
	opLess:         "OP_LESS",
	opAdd:          "OP_ADD",
	opSubtract:     "OP_SUBSTRACT",
	opMultiply:     "OP_MULTIPLY",
	opDivide:       "OP_DIVIDE",
	opNot:          "OP_NOT",
	opNegate:       "OP_NEGATE",
	opPrint:        "OP_PRINT",
	opJump:         "OP_JUMP",
	opJumpIfFalse:  "OP_JUMP_IF_FALSE",
	opLoop:         "OP_LOOP",
	opCall:         "OP_CALL",
	opClosure:      "OP_CLOSURE",
	opCloseUpvalue: "OP_CLOSE_UPVALUE",
	opReturn:       "OP_RETURN",
	opClass:        "OP_CLASS",
	opGetProperty:  "OP_GET_PROPERTY",
	opSetProperty:  "OP_SET_PROPERTY",
	opMethod:       "OP_METHOD",
	opInherit:      "OP_INHERIT",
	opImport:       "OP_IMPORT",
	opInvoke:       "OP_INVOKE",
	opSuperInvoke:  "OP_SUPER_INVOKE",
	opThread:       "OP_THREAD",
	opResume:       "OP_RESUME",
	opYield:        "OP_YIELD",
	opArray:        "OP_ARRAY",
	opSetIndex:     "OP_SET_INDEX",
	opGetIndex:     "OP_GET_INDEX",
	opMap:          "OP_MAP",
}
