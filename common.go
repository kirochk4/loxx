package loxx

import (
	"fmt"
	"math"
	"strings"
)

const Version = "0.0.0"

const (
	eofByte byte = 0

	uint8Max  uint8  = math.MaxUint8
	uint16Max uint16 = math.MaxUint16

	uint8Count int = math.MaxUint8 + 1
)

const (
	debugTraceExecution = false
	debugPrintCode      = false
	debugPrintTokens    = false

	modeAutoSemicolons = true
)

const (
	stringInit  = "init"
	stringThis  = "this"
	stringSuper = "super"

	loxxFileExt = ".loxx"
)

func boolToUint8(b bool) uint8 {
	if b {
		return 1
	}
	return 0
}

func cover(str string, width int, c string) string {
	left := (width - len(str)/2) - 1
	var right int
	if len(str)%2 == 0 {
		right = left
	} else {
		right = left + 1
	}
	return fmt.Sprintf(
		"%s %s %s",
		multiplyString(c, left),
		str,
		multiplyString(c, right),
	)
}

func multiplyString(str string, m int) string {
	var res strings.Builder
	for range m {
		res.WriteString(str)
	}
	return res.String()
}

func shortString(str string, length int, cutNewLine bool) string {
	rStr := []rune(str)
	if cutNewLine {
		i := runesIndex(rStr, '\r')
		if i < 0 {
			i = runesIndex(rStr, '\r')
		}
		if i >= 0 && (length < 0 || i < length) {
			return string(rStr[:i])
		}
	}
	if length >= 0 && length < len(rStr) {
		return string(rStr[:length])
	}
	return str
}

func runesIndex(runes []rune, target rune) int {
	for i, r := range runes {
		if r == target {
			return i
		}
	}
	return -1
}
