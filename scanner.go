package loxx

import (
	"fmt"
	"strings"
)

type tokenType int

const (
	// single
	tokenLeftParen tokenType = iota
	tokenRightParen
	tokenLeftBrace
	tokenRightBrace
	tokenLeftBracket
	tokenRightBracket
	tokenComma
	tokenMinus
	tokenPlus
	tokenSemicolon
	tokenSlash
	tokenStar
	tokenDot
	tokenBang
	tokenQuestion
	tokenColon
	// double
	tokenBangEqual
	tokenEqual
	tokenEqualEqual
	tokenGreater
	tokenGreaterEqual
	tokenLess
	tokenLessEqual
	// special
	tokenIdentifier
	tokenString
	tokenNumber
	// keywords
	tokenBreak
	tokenContinue
	tokenAnd
	tokenElse
	tokenFalse
	tokenFor
	tokenFun
	tokenClass
	tokenSuper
	tokenThis
	tokenIf
	tokenNil
	tokenOr
	tokenPrint
	tokenReturn
	tokenTrue
	tokenVar
	tokenWhile
	tokenImport
	tokenFrom
	tokenYield
	tokenResume
	tokenWith

	tokenError
	tokenEof

	tokensCount = iota
)


type token struct {
	tokenType
	line    int
	literal string
}

func (t token) String() string {
	return fmt.Sprintf(
		"%04d %-12s '%s'",
		t.line,
		tokenNames[t.tokenType],
		shortString(t.literal, 32, true),
	)
}

type scanner struct {
	source []byte
	sp     int // source pointer
	start  int
	line   int
	semi   bool
}

func newScanner(source []byte) scanner {
	return scanner{
		source: source,
		sp:     0,
		line:   1,
		semi:   false,
	}
}

func (s *scanner) scanToken() token {
	startLine := s.line
	s.skipWhitespace()

	s.start = s.sp

	if modeAutoSemicolons {
		if s.semi && (startLine < s.line || s.isAtEnd()) {
			return s.makeToken(tokenSemicolon)
		}
	}

	if s.isAtEnd() {
		return s.makeToken(tokenEof)
	}

	char := s.advance()

	if isAlpha(char) {
		return s.identifier()
	}
	if isDigit(char) {
		if char == '0' {
			switch lowerChar(s.current()) {
			case 'x':
				return s.integer(16)
			case 'o':
				return s.integer(8)
			case 'b':
				return s.integer(2)
			}
		}
		return s.float()
	}
	switch char {
	case '(':
		return s.makeToken(tokenLeftParen)
	case ')':
		return s.makeToken(tokenRightParen)
	case '{':
		return s.makeToken(tokenLeftBrace)
	case '}':
		return s.makeToken(tokenRightBrace)
	case '[':
		return s.makeToken(tokenLeftBracket)
	case ']':
		return s.makeToken(tokenRightBracket)
	case ';':
		return s.makeToken(tokenSemicolon)
	case ',':
		return s.makeToken(tokenComma)
	case '-':
		return s.makeToken(tokenMinus)
	case '+':
		return s.makeToken(tokenPlus)
	case '/':
		return s.makeToken(tokenSlash)
	case '*':
		return s.makeToken(tokenStar)
	case '?':
		return s.makeToken(tokenQuestion)
	case ':':
		return s.makeToken(tokenColon)
	case '.':
		return s.makeToken(tokenDot)
	case '!':
		t := tokenBang
		if s.match('=') {
			t = tokenBangEqual
		}
		return s.makeToken(t)
	case '=':
		t := tokenEqual
		if s.match('=') {
			t = tokenEqualEqual
		}
		return s.makeToken(t)
	case '<':
		t := tokenLess
		if s.match('=') {
			t = tokenLessEqual
		}
		return s.makeToken(t)
	case '>':
		t := tokenGreater
		if s.match('=') {
			t = tokenGreaterEqual
		}
		return s.makeToken(t)
	case '"':
		return s.string()
	}
	return s.errorToken("Unexpected character.")
}

func (s *scanner) isAtEnd() bool {
	return s.current() == eofByte
}

func (s *scanner) current() byte {
	if s.sp >= len(s.source) {
		return eofByte
	}
	return s.source[s.sp]
}

func (s *scanner) previous() byte {
	if s.sp-1 < 0 {
		return eofByte
	}
	return s.source[s.sp-1]
}

func (s *scanner) peek() byte {
	if s.sp+1 >= len(s.source) {
		return eofByte
	}
	return s.source[s.sp+1]
}

func (s *scanner) advance() byte {
	ret := s.current()
	s.sp++
	return ret
}

func (s *scanner) match(expect byte) bool {
	if s.isAtEnd() {
		return false
	}
	if s.current() != expect {
		return false
	}
	s.sp++
	return true
}

func (s *scanner) makeToken(t tokenType) token {
	switch t {
	case tokenIdentifier, tokenNumber, tokenString,
		tokenNil, tokenFalse, tokenTrue,
		tokenBreak, tokenContinue, tokenReturn,
		tokenRightParen, tokenRightBracket, tokenRightBrace:
		s.semi = true
	default:
		s.semi = false
	}
	var literal string
	if t == tokenString {
		if l, ok := escapeString(s.source[s.start:s.sp]); !ok {
			return s.errorToken("Invalid escape sequence.")
		} else {
			literal = l
		}
	} else {
		literal = string(s.source[s.start:s.sp])
	}
	tk := token{t, s.line, literal}
	if debugPrintTokens {
		fmt.Println(tk)
	}
	return tk
}

func (s *scanner) errorToken(message string) token {
	return token{
		tokenType: tokenError,
		line:      s.line,
		literal:   message,
	}
}

func (s *scanner) skipWhitespace() {
	for {
		char := s.current()
		switch char {
		case ' ', '\r', '\t':
			s.advance()
		case '\n':
			s.line++
			s.advance()
		case '/':
			if s.peek() == '/' {
				for s.current() != '\n' && !s.isAtEnd() {
					s.advance()
				}
			} else {
				return
			}
		default:
			return
		}
	}
}

func (s *scanner) identifierType() tokenType {
	if t, ok := keywords[string(s.source[s.start:s.sp])]; ok {
		return t
	}
	return tokenIdentifier
}

func (s *scanner) identifier() token {
	for isAlpha(s.current()) || isDigit(s.current()) {
		s.advance()
	}
	return s.makeToken(s.identifierType())
}

func (s *scanner) float() token {
	allowUnderscore := true
	for isDigit(s.current()) || (allowUnderscore && s.current() == '_') {
		allowUnderscore = s.current() != '_'
		s.advance()
	}
	if s.current() == '.' && isDigit(s.peek()) && allowUnderscore {
		s.advance()
		for isDigit(s.current()) || (allowUnderscore && s.current() == '_') {
			allowUnderscore = s.current() != '_'
			s.advance()
		}
	}
	if !allowUnderscore {
		if s.current() == '_' {
			return s.errorToken("Double underscore in number literal.")
		}
		return s.errorToken("Number literal ends with underscore.")
	}
	return s.makeToken(tokenNumber)
}

func (s *scanner) integer(base int) token {
	s.advance()
	if base <= 10 {
		max := byte('0' + base)
		for isDigit(s.current()) {
			if s.current() >= max {
				return s.errorToken("Invalid symbol in number literal.")
			}
			s.advance()
		}
	} else {
		for isHex(s.current()) {
			s.advance()
		}
	}
	return s.makeToken(tokenNumber)
}

func (s *scanner) string() token {
	for !(s.current() == '"' && s.previous() != '\\') && !s.isAtEnd() {
		if s.current() == '\n' {
			s.line++
		}
		s.advance()
	}
	if s.isAtEnd() {
		return s.errorToken("Unterminated string.")
	}
	s.advance()
	return s.makeToken(tokenString)
}

func isAlpha(char byte) bool {
	return 'a' <= char && char <= 'z' ||
		'A' <= char && char <= 'Z' ||
		char == '_'
}

func isDigit(char byte) bool {
	return '0' <= char && char <= '9'
}

func isHex(char byte) bool {
	return isDigit(char) || 'a' <= lowerChar(char) && lowerChar(char) <= 'f'
}

func lowerChar(char byte) byte {
	return ('a' - 'A') | char
}

func escapeString(source []byte) (string, bool) {
	var result strings.Builder
	for i := 0; i < len(source); i++ {
		b := source[i]
		if b == '\\' {
			if i+1 == len(source) {
				return "", false
			}
			switch source[i+1] {
			case 'n':
				result.WriteByte('\n')
			case 'r':
				result.WriteByte('\r')
			case 't':
				result.WriteByte('\t')
			case 'b':
				result.WriteByte('\b')
			case '\\':
				result.WriteByte('\\')
			case '"':
				result.WriteByte('"')
			default:
				return "", false
			}
			i++
			continue
		}
		result.WriteByte(b)
	}
	return result.String(), true
}

var keywords = map[string]tokenType{
	"and":      tokenAnd,
	"else":     tokenElse,
	"false":    tokenFalse,
	"for":      tokenFor,
	"fun":      tokenFun,
	"if":       tokenIf,
	"nil":      tokenNil,
	"or":       tokenOr,
	"print":    tokenPrint,
	"return":   tokenReturn,
	"true":     tokenTrue,
	"var":      tokenVar,
	"while":    tokenWhile,
	"class":    tokenClass,
	"super":    tokenSuper,
	"this":     tokenThis,
	"break":    tokenBreak,
	"continue": tokenContinue,
	"import":   tokenImport,
	"from":     tokenFrom,
	"yield":    tokenYield,
	"resume":   tokenResume,
	"with":     tokenWith,
}

var tokenNames = [...]string{
	tokenLeftParen:    "left paren",
	tokenRightParen:   "right paren",
	tokenLeftBrace:    "left brace",
	tokenRightBrace:   "left brace",
	tokenLeftBracket:  "left bracket",
	tokenRightBracket: "left bracket",
	tokenComma:        "comma",
	tokenMinus:        "minus",
	tokenPlus:         "plus",
	tokenSemicolon:    "semicolon",
	tokenSlash:        "slash",
	tokenStar:         "star",
	tokenDot:          "dot",
	tokenBang:         "bang",
	tokenQuestion:     "question",
	tokenColon:        "colon",
	tokenBangEqual:    "bang equal",
	tokenEqual:        "equal",
	tokenEqualEqual:   "equal equal",
	tokenGreater:      "greater",
	tokenGreaterEqual: "greater equal",
	tokenLess:         "less",
	tokenLessEqual:    "less equal",
	tokenIdentifier:   "identifier",
	tokenString:       "string",
	tokenNumber:       "number",
	tokenBreak:        "break",
	tokenContinue:     "continue",
	tokenAnd:          "and",
	tokenElse:         "else",
	tokenFalse:        "false",
	tokenFor:          "for",
	tokenFun:          "fun",
	tokenClass:        "class",
	tokenSuper:        "super",
	tokenThis:         "this",
	tokenIf:           "if",
	tokenNil:          "nil",
	tokenOr:           "or",
	tokenPrint:        "print",
	tokenReturn:       "return",
	tokenTrue:         "true",
	tokenVar:          "var",
	tokenWhile:        "while",
	tokenImport:       "import",
	tokenFrom:         "from",
	tokenYield:        "yield",
	tokenResume:       "resume",
	tokenWith:         "with",

	tokenError: "__error",
	tokenEof:   "__eof",
}
