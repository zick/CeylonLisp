import ceylon.collection { HashMap }

Character kLPar = '(';
Character kRPar = ')';
Character kQuote = '\'';

class ConsT<T>(T a, T d) {
  shared variable T car = a;
  shared variable T cdr = d;
}
alias SubrT<T> => Callable<T, [T]>;
alias ExprT<T> => [T, T, T];
alias DataT<T> => Number|String|ConsT<T>|SubrT<T>|ExprT<T>;

class LObj(String t, DataT<LObj> d) {
  shared String tag = t;
  shared DataT<LObj> data = d;
}

alias Cons => ConsT<LObj>;
alias Subr => SubrT<LObj>;
alias Expr => ExprT<LObj>;
alias Data => DataT<LObj>;

LObj kNil = LObj("nil", "nil");

LObj safeCar(LObj obj) {
  Data data = obj.data;
  switch (data)
  case (is Cons) {
    return data.car;
  }
  else { return kNil; }
}
LObj safeCdr(LObj obj) {
  Data data = obj.data;
  switch (data)
  case (is Cons) {
    return data.cdr;
  }
  else { return kNil; }
}


LObj makeError(String str) => LObj("error", str);

HashMap<String, LObj> sym_table = HashMap<String, LObj>();
LObj makeSym(String str) {
  if (exists sym =  sym_table.get(str)) {
    return sym;
  } else {
    LObj ret = LObj("sym", str);
    sym_table.put(str, ret);
    return ret;
  }
}

LObj makeNum(Number num) => LObj("num", num);

LObj makeCons(LObj a, LObj d) => LObj("cons", ConsT<LObj>(a, d));

LObj makeSubr(Subr fn) => LObj("subr", fn);

LObj makeExpr(LObj args, LObj env) =>
  LObj("expr", [safeCar(args), safeCdr(args), env]);

LObj nreverse(variable LObj lst) {
  variable LObj ret = kNil;
  while (is Cons cons = lst.data) {
    LObj tmp = cons.cdr;
    cons.cdr = ret;
    ret = lst;
    lst = tmp;
  }
  return ret;
}

LObj pairlis(variable LObj lst1, variable LObj lst2) {
  variable LObj ret = kNil;
  while (is Cons c1 = lst1.data, is Cons c2 = lst2.data) {
    ret = makeCons(makeCons(c1.car, c2.car), ret);
    lst1 = c1.cdr;
    lst2 = c2.cdr;
  }
  return nreverse(ret);
}

Boolean isSpace(Character c) {
  return c in "\n\r\t ";
}

Boolean isDelimiter(Character c) {
  return c == kLPar || c == kRPar || c == kQuote || isSpace(c);
}

String skipSpaces(String str) {
  return str.trimLeading(isSpace);
}

LObj makeNumOrSym(String str) {
  variable Integer i = 0;
  variable Integer num = 0;
  variable Boolean negative = false;
  variable Boolean isNumber = false;
  for (c in str) {
    if (i == 0 && c == '-') {
      negative = true;
    } else if (c.digit) {
      num = num * 10 + (c.integer - '0'.integer);
      isNumber = true;
    } else {
      isNumber = false;
      break;
    }
    i++;
  }
  if (isNumber) {
    if (negative) {
      num *= -1;
    }
    return makeNum(num);
  }
  return makeSym(str);
}

class ParseState (LObj o, String s) {
  shared LObj obj = o;
  shared String next = s;
}

ParseState readAtom(variable String str) {
  variable String next = "";
  variable Integer i = 0;
  for (c in str) {
    if (isDelimiter(c)) {
      next = str[i..str.size-1];
      str = str[0..i-1];
      break;
    }
    i++;
  }
  return ParseState(makeNumOrSym(str), next);
}

ParseState read(variable String str) {
  str = skipSpaces(str);
  if (exists c = str[0]) {
    if (c == kRPar) {
      return ParseState(makeError("invalid syntax: " + str), "");
    } else if (c == kLPar) {
      return readList(str[1..str.size-1]);
    } else if (c == kQuote) {
      ParseState tmp = read(str[1..str.size-1]);
      return ParseState(makeCons(makeSym("quote"), makeCons(tmp.obj, kNil)),
                        tmp.next);
    } else {
      return readAtom(str);
    }
  } else {
    return ParseState(makeError("empty input"), "");
  }
}

ParseState readList(variable String str) {
  variable LObj ret = kNil;
  while (true) {
    str = skipSpaces(str);
    if (exists c = str[0]) {
      if (c == kRPar) {
        break;
      }
    } else {
      return ParseState(makeError("unfinished parenthesis"), "");
    }
    ParseState tmp = read(str);
    if (tmp.obj.tag == "error") {
      return tmp;
    }
    ret = makeCons(tmp.obj, ret);
    str = tmp.next;
  }
  return ParseState(nreverse(ret), str[1..str.size-1]);
}

String printObj(LObj obj) {
  if (is String str = obj.data) {
    if (obj.tag == "error") {
      return "<error: " + str + ">";
    }
    return str;
  } else if (is Number num = obj.data) {
    return num.string;
  } else if (is Cons cons = obj.data) {
    return printList(obj);
  } else if (obj.tag == "subr" || obj.tag == "expr") {
    return "<" + obj.tag + ">";
  }
  return "<unknown>";
}

String printList(variable LObj obj) {
  variable String ret = "";
  variable Boolean first = true;
  while (is Cons cons = obj.data) {
    if (first) {
      first = false;
    } else {
      ret += " ";
    }
    ret += printObj(cons.car);
    obj = cons.cdr;
  }
  if (obj == kNil) {
    return "(" + ret + ")";
  }
  return "(" + ret + " . " + printObj(obj) + ")";
}

LObj findVar(LObj sym, variable LObj env) {
  while (is Cons ec = env.data) {
    variable LObj alist = ec.car;
    while (is Cons ac = alist.data) {
      variable Boolean found = false;
      if (is Cons pair = ac.car.data, pair.car == sym) {
        // I don't know why but we can't return ac.car here. It looks like
        // nested "is" doesn't work.
        found = true;
      }
      if (found) {
        return ac.car;
      }
      alist = ac.cdr;
    }
    env = ec.cdr;
  }
  return kNil;
}

variable LObj g_env = makeCons(kNil, kNil);

void addToEnv(LObj sym, LObj val, LObj env) {
  if (is Cons cons = env.data) {
    cons.car = makeCons(makeCons(sym, val), cons.car);
  }
}

LObj eval(LObj obj, LObj env) {
  if (obj.tag == "nil" || obj.tag == "num" || obj.tag == "error") {
    return obj;
  } else if (obj.tag == "sym") {
    LObj bind = findVar(obj, env);
    if (is Cons pair = bind.data) {
      return pair.cdr;
    } else if (is String str = obj.data) {  // Must be true.
      return makeError(str + " has no value");
    }
  }

  LObj op = safeCar(obj);
  LObj args = safeCdr(obj);
  if (op == makeSym("quote")) {
    return safeCar(args);
  } else if (op == makeSym("if")) {
    if (eval(safeCar(args), env) == kNil) {
      return eval(safeCar(safeCdr(safeCdr(args))), env);
    }
    return eval(safeCar(safeCdr(args)), env);
  } else if (op == makeSym("lambda")) {
    return makeExpr(args, env);
  }
  return apply(eval(op, env), evlis(args, env), env);
}

LObj evlis(variable LObj lst, LObj env) {
  variable LObj ret = kNil;
  while (is Cons cons = lst.data) {
    LObj elm = eval(cons.car, env);
    if (elm.tag == "error") {
      return elm;
    }
    ret = makeCons(elm, ret);
    lst = cons.cdr;
  }
  return nreverse(ret);
}

LObj progn(variable LObj body, LObj env) {
  variable LObj ret = kNil;
  while (is Cons cons = body.data) {
    ret = eval(cons.car, env);
    body = cons.cdr;
  }
  return ret;
}

LObj apply(LObj fn, LObj args, LObj env) {
  if (fn.tag == "error") {
    return fn;
  } else if (args.tag == "error") {
    return args;
  } else if (is Subr subr = fn.data) {
    return subr(args);
  } else if (is Expr expr = fn.data) {
    return progn(expr[1], makeCons(pairlis(expr[0], args), expr[2]));
  }
  return makeError(printObj(fn) + " is not function");
}

LObj subrCar(LObj args) {
  return safeCar(safeCar(args));
}

LObj subrCdr(LObj args) {
  return safeCdr(safeCar(args));
}

LObj subrCons(LObj args) {
  return makeCons(safeCar(args), safeCar(safeCdr(args)));
}

String? readLine() => process.readLine();

void run() {
  addToEnv(makeSym("car"), makeSubr(subrCar), g_env);
  addToEnv(makeSym("cdr"), makeSubr(subrCdr), g_env);
  addToEnv(makeSym("cons"), makeSubr(subrCons), g_env);
  addToEnv(makeSym("t"), makeSym("t"), g_env);

  while (true) {
    process.write("> ");
    process.flush();
    // HACK: The return type of process.readLine is String but it can return
    // null when it reads EOF. I verified this code works on ceylon 1.0.0.
    if (exists line = readLine()) {
      print(printObj(eval(read(line).obj, g_env)));
    } else {
      break;
    }
  }
}
