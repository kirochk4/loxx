package loxx

import (
	"fmt"
	"strconv"
	"strings"
)

type Value interface {
	loxxValue()
}

type Nil struct{}

type Boolean bool

type Number float64

type String string

type function struct {
	name         String
	arity        int
	chunk        chunk
	upvalueCount int
}

type upvalue struct {
	location int
	place    *([]Value)
	closed   Value
	next     *upvalue
}

type Closure struct {
	function *function
	upvalues []*upvalue
	module   *Instance
}

type nativeFunction func(this Value, args []Value) (Value, error)

type NativeFunction struct {
	arity    int
	function nativeFunction
}

type Class struct {
	name    String
	methods map[String]Value
}

type Instance struct {
	class  *Class
	fields map[String]Value
}

type BoundMethod struct {
	method   Value
	reciever Value
}

type Array []Value

type Map map[Value]Value

type Box map[String]any

type threadStatus uint8

const (
	ThreadStatusNew threadStatus = iota
	ThreadStatusRunning
	ThreadStatusSuspended
	ThreadStatusDone
)

type Thread struct {
	closure      *Closure
	frames       []callFrame
	stack        []Value
	status       threadStatus
	openUpvalues *upvalue
}

func (t *Thread) reset() {
	t.stack = t.stack[:0]
	t.frames = t.frames[:0]
	t.openUpvalues = nil
}

func (b Box) set(name String, value Value) string {
	prop, ok := b[name]
	if !ok {
		return fmt.Sprintf("Undefined property '%s'.", prop)
	}
	switch value := value.(type) {
	case Number:
		switch prop := prop.(type) {
		case *float64:
			*prop = float64(value)
		case *float32:
			*prop = float32(value)
		case *int64:
			*prop = int64(value)
		case *int32:
			*prop = int32(value)
		case *int:
			*prop = int(value)
		case *string:
			*prop = strconv.FormatFloat(float64(value), 'g', -1, 64)
		case *bool:
			*prop = value != 0
		default:
			return "Incompatible value and pointer in box."
		}
	case String:
		switch prop := prop.(type) {
		case *string:
			*prop = string(value)
		case *bool:
			*prop = value != ""
		default:
			return "Incompatible value and pointer in box."
		}
	case Boolean:
		switch prop := prop.(type) {
		case *string:
			*prop = strconv.FormatBool(bool(value))
		case *bool:
			*prop = bool(value)
		default:
			return "Incompatible value and pointer in box."
		}
	default:
		return "Incompatible value in box."
	}
	return ""
}

func (b Box) get(name String) (Value, string) {
	prop, ok := b[name]
	if !ok {
		return nil, fmt.Sprintf("Undefined property '%s'.", prop)
	}
	switch prop := prop.(type) {
	case *float64:
		return Number(*prop), ""
	case *float32:
		return Number(*prop), ""
	case *int64:
		return Number(*prop), ""
	case *int32:
		return Number(*prop), ""
	case *int:
		return Number(*prop), ""
	case *string:
		return String(*prop), ""
	case *bool:
		return Boolean(*prop), ""
	default:
		return nil, "Incompatible pointer in box."
	}
}

func (a *Array) checkIndex(valIndex Value) (int, string) {
	numIndex, ok := valIndex.(Number)
	if !ok {
		return 0, "Index must be number."
	}
	index := int(numIndex)
	if Number(index) != numIndex {
		return 0, "Index must be integer."
	}
	if index < 0 {
		index = len(*a) + index
	}
	if index > len(*a)-1 || index < 0 {
		return 0, fmt.Sprintf("Index (%d) out of range (%d).", index, len(*a))
	}
	return index, ""
}

func (u *upvalue) getValue() Value {
	if u.location == -1 {
		return u.closed
	}
	return (*u.place)[u.location]
}

func (u *upvalue) setValue(value Value) {
	if u.location == -1 {
		u.closed = value
		return
	}
	(*u.place)[u.location] = value
}

func newInstance(class *Class) *Instance {
	return &Instance{class, make(map[String]Value)}
}

func newClass(name String) *Class {
	return &Class{name, make(map[String]Value)}
}

func newFunction() *function {
	return &function{
		arity:        0,
		upvalueCount: 0,
		chunk:        newChunk(),
		name:         "",
	}
}

func newClosure(f *function, mod *Instance) *Closure {
	return &Closure{
		function: f,
		upvalues: make([]*upvalue, f.upvalueCount),
		module:   mod,
	}
}

func newBoundMethod(method Value, reciever Value) *BoundMethod {
	return &BoundMethod{method, reciever}
}

func newArray(cap int) *Array {
	arr := Array(make([]Value, cap))
	return &arr
}

func newMap(cap int) Map {
	return make(map[Value]Value, cap)
}

func newThread(closure *Closure) *Thread {
	return &Thread{
		closure,
		make([]callFrame, 0, framesMax),
		make([]Value, 0, stackMax),
		ThreadStatusNew,
		nil,
	}
}

func (v Nil) loxxValue()            {}
func (v Boolean) loxxValue()        {}
func (v Number) loxxValue()         {}
func (v String) loxxValue()         {}
func (v NativeFunction) loxxValue() {}
func (v *function) loxxValue()      {}
func (v *Closure) loxxValue()       {}
func (v *Class) loxxValue()         {}
func (v *Instance) loxxValue()      {}
func (v *BoundMethod) loxxValue()   {}
func (v *Thread) loxxValue()        {}
func (v *Array) loxxValue()         {}
func (v Map) loxxValue()            {}
func (v Box) loxxValue()            {}

func valueToBoolean(value Value) Boolean {
	if _, isNil := value.(Nil); isNil {
		return false
	}
	if b, isBool := value.(Boolean); isBool {
		return b
	}
	return true
}

func classOf(value Value) *Class {
	switch value := value.(type) {
	case Nil:
		panic("class of: can't get class of nil")
	case Boolean:
		return classBoolean
	case Number:
		return classNumber
	case String:
		return classString
	case *Closure:
		return classFunction
	case *BoundMethod:
		return classBoundMethod
	case *Thread:
		return classThread
	case *Instance:
		return value.class
	case *Array:
		return classArray
	case Map:
		return classMap
	case Box:
		panic("class of: class of box")
	case *Class:
		return classClass
	default:
		panic("class of: unknown type")
	}
}

func sprintValue(value Value) string {
	switch value := value.(type) {
	case Nil:
		return "nil"
	case Boolean:
		return strconv.FormatBool(bool(value))
	case Number:
		return strconv.FormatFloat(float64(value), 'g', -1, 64)
	case String:
		return string(value)
	case *function:
		if value.name == "" {
			return "<script>"
		}
		return fmt.Sprintf("<fn '%s'>", value.name)
	case *Closure:
		return sprintValue(value.function)
	case *Thread:
		if value.closure.function.name == "" {
			return "<script>"
		}
		return fmt.Sprintf("<tr '%s'>", value.closure.function.name)
	case NativeFunction:
		return "<native fn>"
	case *Class:
		return fmt.Sprintf("<class '%s'>", value.name)
	case *Instance:
		return fmt.Sprintf("<'%s' instance>", value.class.name)
	case *BoundMethod:
		return sprintValue(value.method)
	case *Array:
		var lit strings.Builder
		lit.WriteString("<array ")
		for i, elem := range *value {
			lit.WriteString(sprintValue(elem))
			if i != len(*value)-1 {
				lit.WriteString(", ")
			}
		}
		lit.WriteString(">")
		return lit.String()
	case Map:
		var lit strings.Builder
		lit.WriteString("<map ")
		i := 0
		for key, val := range value {
			lit.WriteString(sprintValue(key))
			lit.WriteString(": ")
			lit.WriteString(sprintValue(val))
			if i != len(value)-1 {
				lit.WriteString(", ")
			}
			i++
		}
		lit.WriteString(">")
		return lit.String()
	case Box:
		var lit strings.Builder
		lit.WriteString("<box ")
		i := 0
		for key, val := range value {
			lit.WriteString(sprintValue(key))
			lit.WriteString(": ")
			lit.WriteString(fmt.Sprint(val))
			if i != len(value)-1 {
				lit.WriteString(", ")
			}
			i++
		}
		lit.WriteString(">")
		return lit.String()
	default:
		return "<error>"
	}
}

func isNil(value Value) bool {
	_, is := value.(Nil)
	return is
}
