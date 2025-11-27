package loxx

import (
	"errors"
	"fmt"
	"maps"
	"os"
	"path"
	"path/filepath"
)

const (
	framesMax int = 64
	stackMax      = framesMax * uint8Count
)

var (
	ErrInterpretRuntimeError = errors.New("loxx runtime error")
	ErrInterpretCompileError = errors.New("loxx compile error")
)

type callFrame struct {
	closure *Closure
	ip      int // instruction pointer
	slots   int // first slot index
}

func (f *callFrame) readByte() uint8 {
	f.ip++
	return f.closure.function.chunk.code[f.ip-1]
}

func (f *callFrame) readShort() uint16 {
	big := f.readByte()
	small := f.readByte()
	return uint16(big)<<8 | uint16(small)
}

func (f *callFrame) readConstant() Value {
	return f.closure.function.chunk.constants[f.readByte()]
}

func (f *callFrame) readString() String {
	return f.readConstant().(String)
}

type VM struct {
	thread    *Thread
	pool      []*Thread
	Globals   map[String]Value
	module    *Instance
	modules   map[string]*Instance
	directory string
}

func New(dir string) *VM {
	dir, _ = filepath.Abs(dir)
	vm := &VM{
		thread:    nil,
		pool:      make([]*Thread, 0),
		Globals:   make(map[String]Value),
		module:    newInstance(classModule),
		modules:   make(map[string]*Instance),
		directory: dir,
	}
	LoadNative(vm, "clock", 0, nativeClock)
	return vm
}

func (vm *VM) Interpret(source []byte) error {
	f := newCompiler(source).compile()
	if f == nil {
		return ErrInterpretCompileError
	}

	if debugPrintCode {
		disassembleFunction(f)
	}

	cl := newClosure(f, vm.module)
	vm.thread = newThread(cl)

	vm.callThread(vm.thread, 0)

	return vm.run()
}

func (vm *VM) Call(fun *Closure, args ...Value) error {
	vm.thread = newThread(fun)

	vm.thread.status = ThreadStatusRunning
	vm.pool = append(vm.pool, vm.thread)
	vm.push(vm.thread.closure)
	vm.thread.stack = append(vm.thread.stack, args...)
	vm.callClosure(vm.thread.closure, len(args))

	return vm.run()
}

func (vm *VM) run() error {
	frame := &vm.thread.frames[len(vm.thread.frames)-1]

	for {
		if debugTraceExecution {
			for _, slot := range vm.thread.stack {
				fmt.Printf("[%s]", sprintValue(slot))
			}
			fmt.Println()
		}

		switch instruction := frame.readByte(); instruction {
		case opConstant:
			constant := frame.readConstant()
			vm.push(constant)
		case opClass:
			vm.push(newClass(frame.readString()))
		case opNil:
			vm.push(Nil{})
		case opTrue:
			vm.push(Boolean(true))
		case opFalse:
			vm.push(Boolean(false))
		case opPop:
			vm.pop()
		case opArray:
			elemCount := int(frame.readByte())
			array := newArray(elemCount)
			copy(*array, vm.thread.stack[len(vm.thread.stack)-elemCount:])
			vm.thread.stack = vm.thread.stack[:len(vm.thread.stack)-elemCount]
			vm.push(array)
		case opMap:
			pairCount := int(frame.readByte())
			m := newMap(pairCount)
			for range pairCount {
				value := vm.pop()
				key := vm.pop()
				m[key] = value
			}
			vm.push(m)
		case opSetIndex:
			value := vm.pop()
			index := vm.pop()
			object := vm.pop()
			switch object := object.(type) {
			case *Array:
				index, err := object.checkIndex(index)
				if err != "" {
					return vm.runtimeError(err)
				}
				(*object)[index] = value
			case Map:
				object[index] = value
			default:
				return vm.runtimeError("Only arrays and maps have indexes.")
			}
			vm.push(value)
		case opGetIndex:
			var value Value
			index := vm.pop()
			object := vm.pop()
			switch object := object.(type) {
			case *Array:
				index, err := object.checkIndex(index)
				if err != "" {
					return vm.runtimeError(err)
				}
				value = (*object)[index]
			case Map:
				if v, ok := object[index]; !ok {
					return vm.runtimeError("Key not exists.")
				} else {
					value = v
				}
			default:
				return vm.runtimeError("Only arrays and maps have indexes.")
			}
			vm.push(value)
		case opGetLocal:
			slot := int(frame.readByte())
			vm.push(vm.thread.stack[frame.slots+slot])
		case opSetLocal:
			slot := int(frame.readByte())
			vm.thread.stack[frame.slots+slot] = vm.peek(0)
		case opGetGlobal:
			name := frame.readString()
			value, ok := frame.closure.module.fields[name]
			if !ok {
				value, ok = vm.Globals[name]
			}
			if !ok {
				return vm.runtimeError("Undefined variable '%s'.", name)
			}
			vm.push(value)
		case opDefineGlobal:
			name := frame.readString()
			frame.closure.module.fields[name] = vm.pop()
		case opSetGlobal:
			name := frame.readString()
			value := vm.peek(0)
			if _, ok := frame.closure.module.fields[name]; ok {
				frame.closure.module.fields[name] = value
			} else if _, ok = vm.Globals[name]; ok {
				vm.Globals[name] = value
			} else {
				return vm.runtimeError("Undefined variable '%s'.", name)
			}
		case opGetUpvalue:
			slot := int(frame.readByte())
			upval := frame.closure.upvalues[slot]
			vm.push(upval.getValue())
		case opSetUpvalue:
			slot := int(frame.readByte())
			upval := frame.closure.upvalues[slot]
			upval.setValue(vm.peek(0))
		case opEqual:
			v2 := vm.pop()
			v1 := vm.pop()
			vm.push(Boolean(v1 == v2))
		case opAdd:
			v1, isString1 := vm.peek(1).(String)
			v2, isString2 := vm.peek(0).(String)
			if isString1 && isString2 {
				vm.pop()
				vm.pop()
				vm.push(v1 + v2)
			} else {
				if err := vm.binaryOp(
					binaryOps[instruction],
					"Operands must be two numbers or two strings.",
				); err != nil {
					return err
				}
			}
		case opGreater, opLess, opSubtract, opMultiply, opDivide:
			if err := vm.binaryOp(binaryOps[instruction], "Operands must be numbers."); err != nil {
				return err
			}
		case opNot:
			vm.push(!valueToBoolean(vm.pop()))
		case opNegate:
			v, isNumber := vm.peek(0).(Number)
			if !isNumber {
				return vm.runtimeError("Operand must be a number.")
			}
			vm.pop()
			vm.push(-v)
		case opPrint:
			fmt.Println(sprintValue(vm.pop()))
		case opJump:
			frame.ip += int(frame.readShort())
		case opJumpIfFalse:
			offset := int(frame.readShort())
			condition := valueToBoolean(vm.peek(0))
			if !condition {
				frame.ip += offset
			}
		case opLoop:
			frame.ip -= int(frame.readShort())
		case opCall:
			argCount := int(frame.readByte())
			if err := vm.callValue(vm.peek(argCount), argCount); err != nil {
				return err
			}
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		case opClosure:
			function := frame.readConstant().(*function)
			closure := newClosure(function, vm.module)
			vm.push(closure)
			for i := range len(closure.upvalues) {
				isLocal := frame.readByte()
				index := int(frame.readByte())
				if isLocal == 1 {
					location := frame.slots + index
					closure.upvalues[i] = vm.captureUpvalue(location)
				} else {
					closure.upvalues[i] = frame.closure.upvalues[index]
				}
			}
		case opThread:
			cls := vm.pop().(*Closure)
			thr := newThread(cls)
			vm.push(thr)
		case opCloseUpvalue:
			vm.closeUpvalues(len(vm.thread.stack) - 1)
			vm.pop()
		case opReturn:
			result := vm.pop()
			vm.closeUpvalues(frame.slots)
			vm.thread.frames = vm.thread.frames[:len(vm.thread.frames)-1]
			if len(vm.thread.frames) == 0 { // thread is done
				vm.pool[len(vm.pool)-1].status = ThreadStatusDone // change status
				vm.pool = vm.pool[:len(vm.pool)-1]                // remove thread
				if len(vm.pool) == 0 {                            // no more threads
					vm.pop()   // pop closure
					return nil // end
				}
				vm.thread = vm.pool[len(vm.pool)-1] // current thread
				vm.pop()                            // pop ended thread
				vm.push(result)
			} else {
				vm.thread.stack = vm.thread.stack[:frame.slots]
				vm.push(result)
			}
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		case opYield:
			vm.thread.status = ThreadStatusSuspended
			yil := vm.pop()
			vm.pool = vm.pool[:len(vm.pool)-1]
			vm.thread = vm.pool[len(vm.pool)-1]
			vm.pop() // pop thread
			vm.push(yil)
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		case opResume:
			if err := vm.resumeThread(); err != nil {
				return err
			}
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		case opGetProperty:
			name := frame.readString()
			value := vm.peek(0)
			if isNil(value) {
				return vm.runtimeError("Can't read property of nil.")
			}
			if instance, isInstance := value.(*Instance); isInstance {
				if property, ok := instance.fields[name]; ok {
					vm.pop()
					vm.push(property)
					continue
				}
			}
			if box, isBox := value.(Box); isBox {
				if property, errStr := box.get(name); errStr != "" {
					return vm.runtimeError(errStr)
				} else {
					vm.pop()
					vm.push(property)
					continue
				}
			}
			class := classOf(value)
			if err := vm.bindMethod(class, name); err != nil {
				return err
			}
		case opSetProperty:
			if box, isBox := vm.peek(1).(Box); isBox {
				if errStr := box.set(frame.readString(), vm.peek(0)); errStr != "" {
					return vm.runtimeError(errStr)
				} else {
					value := vm.pop()
					vm.pop()
					vm.push(value)
					continue
				}
			}
			instance, isInstance := vm.peek(1).(*Instance)
			if !isInstance {
				return vm.runtimeError("Only instances and boxes have fields.")
			}
			instance.fields[frame.readString()] = vm.peek(0)
			value := vm.pop()
			vm.pop()
			vm.push(value)
		case opMethod:
			vm.defineMethod(frame.readString())
		case opInherit:
			superclass, ok := vm.peek(1).(*Class)
			if !ok {
				return vm.runtimeError("Superclass must be a class.")
			}
			subclass := vm.peek(0).(*Class)
			maps.Copy(subclass.methods, superclass.methods)
			vm.pop()
		case opGetSuper:
			name := frame.readString()
			superclass := vm.pop().(*Class)
			if err := vm.bindMethod(superclass, name); err != nil {
				return err
			}
		case opImport:
			name := frame.readString()
			if err := vm.importModule(name); err != nil {
				return err
			}
		case opInvoke:
			method := frame.readString()
			argCount := int(frame.readByte())
			if err := vm.invoke(method, argCount); err != nil {
				return err
			}
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		case opSuperInvoke:
			method := frame.readString()
			argCount := int(frame.readByte())
			superclass := vm.pop().(*Class)
			if err := vm.invokeFromClass(superclass, method, argCount); err != nil {
				return err
			}
			frame = &vm.thread.frames[len(vm.thread.frames)-1]
		}
	}
}

func (vm *VM) push(value Value) {
	vm.thread.stack = append(vm.thread.stack, value)
}

func (vm *VM) pop() Value {
	value := vm.thread.stack[len(vm.thread.stack)-1]
	vm.thread.stack = vm.thread.stack[:len(vm.thread.stack)-1]
	return value
}

func (vm *VM) peek(distance int) Value {
	return vm.thread.stack[len(vm.thread.stack)-1-distance]
}

func (vm *VM) runtimeError(format string, a ...any) error {
	fmt.Fprintf(os.Stderr, format+"\n", a...)

	for i := len(vm.thread.frames) - 1; i >= 0; i-- {
		frame := &vm.thread.frames[i]
		function := frame.closure.function
		line := function.chunk.lines[frame.ip]
		fmt.Fprintf(os.Stderr, "  [line %d] in ", line)
		if function.name == "" {
			fmt.Fprintf(os.Stderr, "script\n")
		} else {
			fmt.Fprintf(os.Stderr, "%s()\n", function.name)
		}
	}

	return ErrInterpretRuntimeError
}

func (vm *VM) invoke(name String, argCount int) error {
	reciever := vm.peek(argCount)
	if isNil(reciever) {
		return vm.runtimeError("Can't read property of nil.")
	}
	instance, ok := reciever.(*Instance)
	if ok {
		if field, ok := instance.fields[name]; ok {
			vm.thread.stack[len(vm.thread.stack)-argCount-1] = field
			return vm.callValue(field, argCount)
		}
	}
	class := classOf(reciever)
	return vm.invokeFromClass(class, name, argCount)
}

func (vm *VM) invokeFromClass(class *Class, name String, argCount int) error {
	method, ok := class.methods[name]
	if !ok {
		return vm.runtimeError("Undefined property '%s'.", name)
	}
	return vm.callValue(method, argCount)
}

func (vm *VM) newModuleVM(dir string) *VM {
	return &VM{
		thread:    nil,
		Globals:   vm.Globals,
		module:    newInstance(classModule),
		modules:   vm.modules,
		directory: path.Join(vm.directory, dir),
	}
}

func (vm *VM) importModule(name String) error {
	if mod, ok := vm.modules[string(name)]; ok {
		vm.module.fields[name] = mod
		return nil
	}
	modulePath := path.Join(vm.directory, string(name)) + loxxFileExt
	if mod, ok := vm.modules[modulePath]; ok {
		vm.module.fields[name] = mod
		return nil
	}
	source, err := os.ReadFile(modulePath)
	if err != nil {
		return vm.runtimeError("Can't import module '%s': %s", name, err)
	}
	modVM := vm.newModuleVM(".")
	if err := modVM.Interpret(source); err != nil {
		return err
	}
	vm.modules[modulePath] = modVM.module
	vm.module.fields[name] = modVM.module
	return nil
}

func (vm *VM) bindMethod(class *Class, name String) error {
	method, ok := class.methods[name]
	if !ok {
		return vm.runtimeError("Undefined property '%s'.", name)
	}
	bm := newBoundMethod(method, vm.peek(0))
	vm.pop()
	vm.push(bm)
	return nil
}

func (vm *VM) defineMethod(name String) {
	method := vm.peek(0)
	class := vm.peek(1).(*Class)
	class.methods[name] = method
	vm.pop()
}

func (vm *VM) captureUpvalue(location int) *upvalue {
	var prev *upvalue = nil
	upval := vm.thread.openUpvalues

	for upval != nil && upval.location > location {
		prev = upval
		upval = upval.next
	}

	if upval != nil && upval.location == location {
		return upval
	}

	new := &upvalue{location, &vm.thread.stack, nil, upval}
	if prev != nil {
		prev.next = new
	} else {
		vm.thread.openUpvalues = new
	}
	return new
}

func (vm *VM) closeUpvalues(last int) {
	for vm.thread.openUpvalues != nil && vm.thread.openUpvalues.location >= last {
		upval := vm.thread.openUpvalues
		upval.closed = (*upval.place)[upval.location]
		upval.location = -1
		vm.thread.openUpvalues = upval.next
	}
}

func (vm *VM) resumeThread() error {
	thread, ok := vm.peek(1).(*Thread)
	if !ok {
		return vm.runtimeError("Can only resume threads.")
	}
	switch thread.status {
	case ThreadStatusDone:
		return vm.runtimeError("Can't resume ended thread.")
	case ThreadStatusNew:
		return vm.runtimeError("Can't resume unstarted thread.")
	case ThreadStatusRunning:
		return vm.runtimeError("Can't resume unsuspended thread.")
	}

	with := vm.pop()

	vm.thread = thread
	vm.thread.status = ThreadStatusRunning
	vm.pool = append(vm.pool, thread)
	vm.push(with)
	return nil
}

func (vm *VM) callValue(value Value, argCount int) error {
	switch callee := value.(type) {
	case *Thread:
		return vm.callThread(callee, argCount)
	case *BoundMethod:
		vm.thread.stack[len(vm.thread.stack)-argCount-1] = callee.reciever
		return vm.callValue(callee.method, argCount)
	case *Class:
		vm.thread.stack[len(vm.thread.stack)-argCount-1] = newInstance(callee)
		if init, ok := callee.methods[stringInit]; ok {
			vm.callClosure(init.(*Closure), argCount)
		} else if argCount != 0 {
			return vm.runtimeError("Expected 0 arguments but got %d.", argCount)
		}
		return nil
	case *Closure:
		return vm.callClosure(callee, argCount)
	case NativeFunction:
		return vm.callNative(callee, argCount)
	default:
		return vm.runtimeError("Can only call functions and classes.")
	}
}

func (vm *VM) callThread(thread *Thread, argCount int) error {
	if argCount != thread.closure.function.arity {
		return vm.runtimeError("Expected %d arguments but got %d.", thread.closure.function.arity, argCount)
	}
	if thread.status == ThreadStatusRunning {
		return vm.runtimeError("Can't start running thread.")
	}
	if thread.status != ThreadStatusNew {
		thread.reset()
	}

	args := vm.thread.stack[len(vm.thread.stack)-argCount:]
	vm.thread.stack = vm.thread.stack[:len(vm.thread.stack)-argCount]

	vm.thread = thread
	vm.thread.status = ThreadStatusRunning
	vm.pool = append(vm.pool, thread)
	vm.push(thread.closure)
	vm.thread.stack = append(vm.thread.stack, args...)
	return vm.callClosure(thread.closure, argCount)
}

func (vm *VM) callClosure(closure *Closure, argCount int) error {
	if argCount != closure.function.arity {
		return vm.runtimeError("Expected %d arguments but got %d.", closure.function.arity, argCount)
	}
	if len(vm.thread.frames) == framesMax {
		return vm.runtimeError("Stack overflow.")
	}
	vm.thread.frames = append(vm.thread.frames,
		callFrame{closure, 0, len(vm.thread.stack) - argCount - 1},
	)
	return nil
}

func (vm *VM) callNative(function NativeFunction, argCount int) error {
	if argCount != function.arity {
		return vm.runtimeError("Expected %d arguments but got %d.", function.arity, argCount)
	}
	result, err := function.function(
		vm.thread.stack[len(vm.thread.stack)-argCount-1], // this or function itself
		vm.thread.stack[len(vm.thread.stack)-argCount:],  // args
	)
	if err != nil {
		return vm.runtimeError("In native function: %s.", err)
	}
	vm.thread.stack = vm.thread.stack[:len(vm.thread.stack)-argCount-1]
	vm.push(result)
	return nil
}

func (vm *VM) binaryOp(f func(a, b Number) Value, message string) error {
	v1, isNumber1 := vm.peek(1).(Number)
	v2, isNumber2 := vm.peek(0).(Number)
	if !isNumber1 || !isNumber2 {
		return vm.runtimeError(message)
	}
	vm.pop()
	vm.pop()
	vm.push(f(v1, v2))
	return nil
}

var binaryOps = map[uint8]func(a, b Number) Value{
	opGreater:  func(a, b Number) Value { return Boolean(a > b) },
	opLess:     func(a, b Number) Value { return Boolean(a < b) },
	opAdd:      func(a, b Number) Value { return a + b },
	opSubtract: func(a, b Number) Value { return a - b },
	opMultiply: func(a, b Number) Value { return a * b },
	opDivide:   func(a, b Number) Value { return a / b },
}
