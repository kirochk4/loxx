package loxx

import (
	"errors"
	"math"
	"time"
)

func LoadNative(vm *VM, name string, arity int, fn nativeFunction) {
	f := NativeFunction{
		arity:    arity,
		function: fn,
	}
	vm.Globals[String(name)] = f
}

func NewNativeFunction(arity int, fn nativeFunction) NativeFunction {
	return NativeFunction{
		arity:    arity,
		function: fn,
	}
}

var (
	classBoolean = &Class{"Boolean", make(map[String]Value)}
	classNumber  = &Class{"Number", map[String]Value{
		"power": NativeFunction{1, methodNumberPower},
	}}
	classString = &Class{"String", map[String]Value{
		"reverse": NativeFunction{0, methodStringReverse},
	}}
	classFunction    = &Class{"Function", make(map[String]Value)}
	classBoundMethod = &Class{"Method", make(map[String]Value)}
	classArray       = &Class{"Array", map[String]Value{
		"length": NativeFunction{0, methodArrayLength},
		"append": NativeFunction{1, methodArrayAppend},
	}}
	classMap = &Class{"Map", map[String]Value{
		"length": NativeFunction{0, methodMapLength},
		"get":    NativeFunction{2, methodMapGet},
	}}
	classThread = &Class{"Thread", map[String]Value{
		"status": NativeFunction{0, methodThreadStatus},
	}}
	classClass = &Class{"Class", make(map[String]Value)}

	classModule = &Class{"Module", make(map[String]Value)}
)

func nativeClock(_ Value, _ []Value) (Value, error) {
	return Number(float64(time.Now().UnixNano()) / float64(time.Second)), nil
}



func methodNumberPower(this Value, args []Value) (Value, error) {
	thisNumber := this.(Number)
	p, isNumber := args[0].(Number)
	if !isNumber {
		return nil, errors.New("argument must be number")
	}
	return Number(math.Pow(float64(thisNumber), float64(p))), nil
}

func methodStringReverse(this Value, args []Value) (Value, error) {
	thisString := this.(String)
	result := []rune(thisString)
	for i := 0; i < len(result)/2; i++ {
		result[i], result[len(result)-i-1] = result[len(result)-i-1], result[i]
	}
	return String(result), nil
}

func methodThreadStatus(this Value, args []Value) (Value, error) {
	thisThread := this.(*Thread)
	var status String
	switch thisThread.status {
	case ThreadStatusNew:
		status = "new"
	case ThreadStatusRunning:
		status = "running"
	case ThreadStatusSuspended:
		status = "suspended"
	case ThreadStatusDone:
		status = "done"
	}
	return status, nil
}

func methodArrayLength(this Value, args []Value) (Value, error) {
	thisArray := this.(*Array)
	return Number(len(*thisArray)), nil
}

func methodArrayAppend(this Value, args []Value) (Value, error) {
	thisArray := this.(*Array)
	*thisArray = append(*thisArray, args[0])
	return args[0], nil
}

func methodMapLength(this Value, args []Value) (Value, error) {
	thisMap := this.(Map)
	return Number(len(thisMap)), nil
}

func methodMapGet(this Value, args []Value) (Value, error) {
	thisMap := this.(Map)
	if val, ok := thisMap[args[0]]; ok {
		return val, nil
	}
	return args[1], nil
}
