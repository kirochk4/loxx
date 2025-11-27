# Loxx

## Usage

### Go

```go
import "github.com/kirochk4/loxx"

var events = make(map[string]*loxx.Closure)
var clicks int = 0

// create vm instance
vm := loxx.New(".")

// create native function
addEvent := func(this loxx.Value, args []loxx.Value) (loxx.Value, error) {
    name := args[0].(loxx.String)
    fun := args[1].(*loxx.Closure)
    events[string(name)] = fun
    return loxx.Nil{}, nil
}

// define native function
vm.Globals["addEvent"] = loxx.NewNativeFunction(2, addEvent)

// define outer reference
vm.Globals["outer"] = loxx.Box{
    "clicks": &clicks,
}

// load and interpret your script
source, _ := os.OpenFile("./your_script.loxx")
vm.Interpret(source)

// call added event function
vm.Call(events["onclick"])
```

### Loxx

```js
fun onClick() {
    print "click"

    // this will change outer value
    outer.clicks = outer.clicks + 1
}

addEvent("onclick", onClick)
```
