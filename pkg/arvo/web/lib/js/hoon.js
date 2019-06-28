CodeMirror.defineMode("hoon", function() {
  glyph = /[+\-|$%:.#^~;=?!_,&\/<>%*]/
  term = /^[$&|]|^[a-z]([a-z0-9\-]*[a-z0-9])?/
  num = /~[a-z0-9._~-]+|-?-?^[0-9]([0-9.]*|[xwbv]?[0-9a-zA-Z.-~]*)/
  res = {}
  res.startState = function(){return {soblock: false, doqblock:false, sail:false, space:true}}
  var propOrVar = function(c){
      if(c == '.')
        return 'property'
      return 'variable'
  }
  res.token = function(stream, state){
    if(state.soqblock && stream.sol()){
      if(stream.match(/\s*'''/)){
        state.soqblock = false
      }
      else {
        stream.skipToEnd()
      }
      return "string"
    }
    if(state.doqblock){
      if(stream.match(/\s*"""/)){
        state.doqblock = false
      }
      else {
        stream.skipToEnd()
      }
      return "string"
    }

    if(stream.sol())
      state.space = true

    if(state.sail){
      if(stream.peek().match(/[^#./() ]/)||stream.eol()){
        state.sail = false
        if(stream.match(/:? /)){
          stream.skipToEnd()
          return 'string'
        }
        if(stream.match(term))
          state.sail = true
          return;
        if(stream.match(':'))
          state.sail = true
          return 'operator'
      }
    }
    if(stream.match("'")){
      if(stream.match("''")){
        state.soqblock = true
        return 'string'
      }
      while(stream.match(/^[^'\\]/) || stream.match(/\\./));
      stream.eat("'")
      return 'string'
    }
    if(stream.match('"')){
      if(stream.match('""')){
        state.doqblock = true
        stream.skipToEnd()
        return 'string'
      }
      while(stream.match(/^[^"\\]/) || stream.match(/\\./));
      stream.eat('"')
      return 'string'
    }
    if(stream.match(' ;')){
      if(stream.eat(' ')){
        stream.skipToEnd()
        return 'string'
      }
      if(!stream.match(glyph)){
        state.sail = true
      }
      return 'builtin'
    }

    if(stream.match('::')){
      stream.skipToEnd()
      return 'comment'
    }

    if(stream.match('++  ') || stream.match('+-  ')){
      stream.match(term)
      return 'header'
    }
    if(state.space && stream.match('--')){
      if(stream.eat(glyph) || stream.eat(/[a-z0-9]/))
        stream.backUp(3)
      else return 'header'
    }

    if(stream.match(/^@[a-z]*[A-Z]?/))
      return 'atom'
    if(stream.match(num))
      return 'number'

    if(stream.eat(/[%$]/))
      if(stream.match(term) || stream.match(num) || stream.match('~'))
        return 'tag'
      else stream.backUp(1)
    if(state.space && stream.match('==')){
      return 'tag'
    }

    if(stream.eat('~')){
      if(/[()]/.exec(stream.peek()))
        return 'builtin'
      return 'tag'
    }

    if(stream.eat(/[+\-]/)){
      while(stream.eat(/[<>]/) && stream.eat(/[+\-]/));
      return propOrVar(stream.peek())
    }

    if(stream.eat('`')){
      state.space = false
      return 'operator'
    }
    if(stream.sol() && stream.eatWhile(glyph)){
      state.space = false
      return 'builtin'
    }
    if(stream.eat(glyph)){
      state.space = false
      stream.backUp(2)
      if(stream.eat(/[ ([{]/) || (stream.peek().match(/[^+\-<>]/)
                             && stream.eat(glyph))){  //  expression start
        stream.eatWhile(glyph)
        return 'builtin'
      }
      stream.next()
      if(state.space && stream.eat('=')){
        if(/[()]/.exec(stream.peek()))
          return 'builtin'
        return 'operator'
      }
      if(stream.eat(/[=:.^/]/))
        return 'operator'
      stream.next()
      return 'builtin'
    }

    if(stream.match(term)){
      if(state.space && stream.match('+'))
        return 'tag'
      state.space = false
      return propOrVar(stream.peek())
    }
    if(stream.eat(/[ \[({]/)){
      state.space = true
      return
    }
    stream.next()
  }
  res.lineComment = '::'
  res.fold = "indent"
  return res
});

CodeMirror.defineMIME("text/x-hoon", "hoon");
