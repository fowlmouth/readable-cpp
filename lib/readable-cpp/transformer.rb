
module LazyPP
Transform = Parslet::Transform.new {
  rule(ident: simple(:i)) { i.to_s }
  rule(ident: simple(:i), generics: simple(:g)) {
    GenericIdent.new(i, g)
  }
  rule(ident: simple(:i), generics: sequence(:g)) {
    GenericIdent.new i, g
  }
  rule(expr: simple(:x), generics: simple(:g)) {
    GenericIdent.new x,g
  }
  rule(expr: simple(:x), generics: sequence(:g)) {
    GenericIdent.new x,g
  }
  rule(void: simple(:v)) { 'void' }
  rule(any: simple(:__)) { '...' }
  rule(ptr: simple(:p)) { :ptr }
  rule(ref: simple(:r)) { :ref }
  rule(list_item: simple(:i)){ i }

  rule(ident_eq: { ident: simple(:i), dot_expr: simple(:x) }) {
    InfixExpr.new(i, '=', x)
  }
  rule(ident_eq: simple(:i)) { i }
  rule(stmts: sequence(:s)) {
    StmtList.new s
  }
  rule(stmts: simple(:s)) {
    StmtList.new Array.wrap s
  }
  rule(anon_decl: simple(:ty)) {
    AnonDecl.new ty
  }
  rule(bracket_stmts: sequence(:s)) {
    BracketStmts.new s
  }
  rule(wchar: simple(:w), string: simple(:s)) {
    StringLit.new(w, s.to_s)
  }
  rule(wchar: simple(:w), string: sequence(:s)) {
    StringLit.new(w, s.join(''))
  }
  rule(goto: simple(:l)) { GotoLabel.new :goto, l }
  rule(label: simple(:g)){ GotoLabel.new :label,g }
  # rule(type: {derived: subtree(:d), base: subtree(:b), storage: simple(:s)}){
  #   Type.new(b, Array.wrap(d), s)
  # }
  # rule(type: {derived: subtree(:d), base: subtree(:b)}) {
  #   Type.new b, Array.wrap(d), nil
  # }
  rule(derived: subtree(:d), base: subtree(:b)) {
    Type.new b, d, nil
  }
  rule(derived: subtree(:d), base: subtree(:b), storage: subtree(:s)) {
    Type.new b, d, s
  }
  rule(type: simple(:t)) {t}

  rule(union: { members: subtree(:m) }) {
    UnionType.new(m)
  }
  rule(auto_decl: {name: simple(:n), value: subtree(:v)}) {
    AutoDecl.new(n, v)
  }
  rule(:catch => simple(:c), body: simple(:b)) {
    CatchStmt.new(c, b)
  }
  rule(attempt: simple(:at), catches: sequence(:c)) {
    TryStmt.new(at, c)
  }
  rule(enum: { fields: subtree(:f) }) {
    EnumType.new f
  }
  rule(struct: simple(:body)) {
    StructType.new body
  }
  rule(lang_section: subtree(:l)) {
    #binding.pry
    LangSection.new(l) }
  rule(id_names: sequence(:n), type: simple(:t)) {
    IdentDef.new n, t, nil
  }
  rule(#ident_def: 
  {
    id_names: sequence(:n), type: simple(:t), id_default: simple(:d)}) {
    IdentDef.new n, t, d
  }
  rule(id_names: simple(:n), type: simple(:t)) {
    IdentDef.new n, t, nil
  }
  rule(#ident_def: 
  {
    names: simple(:n), type: simple(:t),
    default: simple(:d)  }) {
    IdentDef.new n, t, d
  }
  rule(ident_def: simple(:i), any: simple(:a)) {[i, '...']}
  rule(type: simple(:t), any: simple(:a)) {[t, '...']}
  rule(ident_def: simple(:_)) {_}
  rule(var_decl: {name: simple(:n), type: simple(:t)}) {
    VarDeclSimple.new(n, t, nil)
  }
  rule(var_decl: {name: simple(:n), type: simple(:t), expr: simple(:x)}){
    VarDeclSimple.new(n, t, x)
  }
  rule(var_decl: {vdi_names: subtree(:n), vdi_type: simple(:t)}) {
    VarDeclInitializer.new n, t
  }
  rule(case_: 'default', body: sequence(:b)) {
    CaseStmt.new :default, b
  }
  rule(case_: simple(:c), body: sequence(:b)) {
    CaseStmt.new [c], b
  }
  rule(case_: sequence(:c), body: sequence(:b)) {
    CaseStmt.new c, b
  }
  rule(switch_stmt: {expr: simple(:x), cases: sequence(:cases)}) {
    SwitchStmt.new x, cases
  }
  rule(name: simple(:n), constructor: subtree(:c)) {
    ConstructorName.new n, c
  }
  rule(import_pkg: subtree(:i)) {
    ImportStmt.new(i)
  }
  rule(define_stmt: {name: simple(:n), expr: simple(:x)}) {
    DefineStmt.new n, x
  }
  rule(include_stmt: { std: simple(:x), file: simple(:f) }) {
    IncludeStmt.new f, :std
  }
  rule(include_stmt: { local: simple(:f) }) {
    IncludeStmt.new f, :local
  }
  rule(include_stmt: sequence(:s)) {
    StmtList.new(s)
  }
  rule(include_stmt: simple(:s)) { s }
  rule(using_stmt: { ns: simple(:ns), using_namespace: simple(:n) }) {
    UsingStmt.new("namespace #{n}")
  }
  # rule(using_stmt: { using_namespace: simple(:n) }) {
  #   UsingStmt.new(n)
  # }
  rule(using_namespace: simple(:n)) { UsingStmt.new n }
  rule(using_stmt: sequence(:s)) { StmtList.new(s) }
  rule(using_stmt: simple(:s)) { s } 
  rule(namespace_decl: {namespace: subtree(:n), body: subtree(:b)}){
    Namespace.new(n, b)
  }
  rule(oper_decl: {
    pos: simple(:pos), name: simple(:name),
    sig: simple(:sig), body: simple(:body) }) { 
    OperDecl.new name, sig, body, pos 
  }
  rule(oper_decl: {
    name: simple(:name), sig: simple(:sig), body: simple(:body)}) {
    OperDecl.new name, sig, body, nil
  }
  rule(func_decl: {
    name: simple(:name),
    generics: simple(:g),
    sig: simple(:sig)  }) { FuncDecl.new name, sig, nil, g }
  rule(func_decl: {
    name: simple(:name), generics: simple(:g),
    sig: simple(:sig), body: simple(:b)}){
    FuncDecl.new name, sig, b, g
  }
  rule(func_decl: {
    name: simple(:name), generics: subtree(:g), specifiers: subtree(:s),
    sig: simple(:sig), body: simple(:b)}) {
    FuncDecl.new name, sig, b, g, s
  }
  rule(func_decl: {
    name: simple(:name), generics: subtree(:g), specifiers: subtree(:s),
    sig: simple(:sig) }) {
    FuncDecl.new name, sig, nil, g, s
  }
  rule(func_decl: {
    name: simple(:name), generics: subtree(:g), specifiers: subtree(:s),
    sig: simple(:sig), eq_0: simple(:eee)}) {
    FuncDecl.new name, sig, :EQ0, g, s
  }
  # rule(func_decl: {
  #   name: simple(:name),
  #   sig: { args: subtree(:args), returns: subtree(:returns) },
  #   body: simple(:b) }) {
  #   FuncDecl.new name, args, returns, b
  # }
  # rule(func_decl: {
  #   name: simple(:name),
  #   sig: { args: subtree(:args), returns: subtree(:returns) }}) {
  #   FuncDecl.new name, args, returns, nil
  # }
  rule(op: simple(:o)) { o.is_a?(Parslet::Slice) ? o.to_s.intern : o }
  rule(float: simple(:f), type: simple(:t)) {
    FloatLiteral.new(f, t)
  }
  rule(cast: { type: simple(:t), expr: simple(:x) }) { CastExpr.new t, x }
  rule(int: simple(:i)) {
    IntLiteral.new i.to_i
  }
  rule(char: simple(:c)) {
    CharLiteral.new c.to_s
  }
  rule(return: simple(:exp)) {
    ReturnStmt.new exp
  }
  rule(ctor_decl: {
    explicit: simple(:e), args: subtree(:args),
    initializers: subtree(:i), body: simple(:b)}) {
    CtorDecl.new args, i, b, e
  }
  rule(dtor_decl: subtree(:d)) {
    DtorDecl.new(d[:body], d[:specifier] || nil)
  }
  rule(parens: simple(:x)) { 
    ParenExpr.new x
  }
  rule(ref: simple(:r), name: simple(:n)) {
    "#{r}#{n}"
  }
  rule(lambda_func: {
    captures: simple(:c), args: subtree(:p), 
    returns: simple(:r), body: simple(:b)
  }) {
    LambdaFunc.new c, p, r, b
  }
  rule(lambda_func: {
    captures: { auto_ref: simple(:___) },
    args: subtree(:p), returns: simple(:r), body: simple(:b)
  }) {
    LambdaFunc.new '&', p, r, b
  }
  rule(lambda_func: {
    captures: {auto_val: simple(:___)},
    args: subtree(:p), returns: simple(:r), body: simple(:b)
  }) {
    LambdaFunc.new '=', p, r, b
  }
  rule(infix: {
    left: simple(:l), op: simple(:o), right: simple(:r)}) {
    InfixExpr.new l, o.to_s, r
  }
  #rule(postfix: sequence(:x)) { PostfixExpr.new x }
  rule(left: simple(:x), access: simple(:a), ident: simple(:i)) {
    PostfixExpr.new x, [a.to_s, i.to_s]
  }
  rule(prefix_op: simple(:o), expr: simple(:x)) {
    PrefixExpr.new o.to_s, x
  }
  rule(ternary: {
    condition: simple(:c), true: simple(:t), false: simple(:f)
  }) {
    TernaryExpr.new c, t, f
  }
  rule(expr: simple(:x), postfix: sequence(:p)) {
    PostfixExpr.new x, p
  }
  rule(expr: simple(:x), postfix: sequence(:p), args: subtree(:a)) {
    PostfixExpr.new x, [*p, FuncCall2.new(a)]
  }
  rule(args: sequence(:a)) {
    FuncCall2.new a
  }
  rule(args: simple(:a)) {
    FuncCall2.new a
  }
  rule(cast: {type: simple(:t), ident: simple(:i)})

  # rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
  #   args: subtree(:a),    infix: simple(:i), right: simple(:x2)) {
  #   InfixExpr.new FuncCall.new(Expr.new(p, x, nil), a), i.to_s.intern, x2
  # }
  # rule(expr: simple(:x), args: subtree(:a), infix: simple(:i), right: simple(:r)) {
  #   InfixExpr.new FuncCall.new(x, a), i.to_s.intern, r
  # }
  # rule(prefix: simple(:p), expr: simple(:x), args: simple(:a)) {
  #   FuncCall.new(Expr.new(p.to_s, x, nil), a)
  # }
  # rule(
  #   prefix: simple(:p), expr: simple(:x),
  #   infix: simple(:i), right: simple(:x2)) {
  #   InfixExpr.new(Expr.new(p, x, nil), i.to_s, x2)
  # }
  # rule(
  #   prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
  #   infix: simple(:i), right: simple(:x2)) {
  #   InfixExpr.new(Expr.new(p, x, pp), i.to_s, x2)
  # }
  # rule(
  #   expr: simple(:x), postfix: simple(:pp),
  #   infix: simple(:i), right: simple(:x2)) {
  #   InfixExpr.new(Expr.new(nil, x, pp), i.to_s, x2)
  # }
  # rule(expr: simple(:x)) {
  #   Expr.new(nil, x, nil)
  # }
  # rule(expr: simple(:x), postfix: simple(:pp)) {
  #   Expr.new(nil, x, pp)
  # }
  # rule(expr: simple(:l), infix: simple(:i), right: simple(:r)) {
  #   InfixExpr.new(l, i.to_s, r)
  # }
  rule(expr: simple(:x)) { 
    x.is_a?(Parslet::Slice) ?
      x.to_s : 
      x 
  }
  rule(expr_stmt: simple(:x)) {
    ExprStmt.new(x)
  }
  # rule(expr: simple(:x), args: subtree(:a)) {
  #   FuncCall.new x, a
  # }
  # rule(parens: simple(:x), args: subtree(:a)) {
  #   FuncCall.new x, a
  # }
  # rule(func: simple(:name), args: subtree(:args)) {
  #   FuncCall.new name, Array.wrap(args)
  # }
  # rule(access: simple(:a), func: simple(:name), args: subtree(:args)) {
  #   FuncCall.new name.prefix(a), Array.wrap(args)
  # }
  rule(type_decl: { 
    name: simple(:n),
    type: { 
      class_type: simple(:c),
      parents: subtree(:p),
      body: subtree(:b) }}) {
    ClassDecl.new(n, c, b, p)
  }
  rule(type_decl: {
    name: simple(:n),
    type: {
      class_type: simple(:c),
      body: subtree(:b) }}) {
    ClassDecl.new(n, c, b, nil)
  }
  rule(type_decl: {
    name: simple(:n),
    enum: { fields: subtree(:f) }}) {
    EnumDecl.new n, f
  }
  rule(struct_lit: simple(:x)) { StructLit.new x }
  rule(struct_lit: sequence(:x)) { StructLit.new x }
  rule(type_decl: {
    name: simple(:n),
    type: simple(:t) }) {
    TypeDecl.new(n, t)
  }
  rule(dot_ident: subtree(:d)) {
    DotName.new d
  }
  # rule(dot_ident: simple(:d)) {
  #   DotName.new d
  # }
  rule(namespace: sequence(:n)) {
    NamespaceIdent.new n
  }
  rule(conditional: {
    kind: simple(:k),
    cond: simple(:c),
    body: simple(:b) }) {
    Conditional.new k, c, b
  }
  rule(for_stmt: {
    l: simple(:l),
    m: simple(:m),
    r: simple(:r),
    body: simple(:b) }) {
    ForStmt.new :for, [l, m, r], b
  }
  rule(conditional: {
    kind: simple(:k), ##else
    body: simple(:b) }) {
    Conditional.new k, nil, b
  }
  rule(range_for: { var: simple(:v), range: simple(:container), body: simple(:b)}) {
    RangeForStmt.new v, container, b
  }
  rule(namespaced: simple(:i)) {
    NamespaceIdent.new i
  }
  rule(namespaced: sequence(:i)) {
    NamespaceIdent.new(i)
  }
  rule(namespaced: {left: simple(:l), right: simple(:r)}) {
    NamespaceIdent.new [l, r]
  }
  rule(modifier: simple(:m), namespaced: simple(:n)) { 
    [m, n]
  }
  rule(modifier: simple(:m), namespaced: sequence(:n)) {
    [m, NamespaceIdent.new(n)]
  }
  rule(sq_bracket_expr: simple(:x)) {
    BracketExpr.new x
  }
  rule(visibility_stmt: simple(:v)) {
    Visibility.new v.to_s
  }
  rule(:throw => simple(:x)) { ThrowStmt.new x }
  rule(access: simple(:a), expr: simple(:x)) {
    AccessExpr.new a, x
  }
  rule(access: simple(:a), ident: simple(:i)) {
    DotName.new([a.to_s, i])
  }
  rule(dot_expr: sequence(:x)) {
    DotExpr.new x
  }
  rule(dot_expr: simple(:x)) {
    DotExpr.new [*x]
  }
}
end