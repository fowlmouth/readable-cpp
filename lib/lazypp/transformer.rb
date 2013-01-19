
module LazyPP
Transform = Parslet::Transform.new {
  rule(ident: simple(:i)) { i.to_s.intern }
  rule(ident: simple(:i), generics: simple(:g)) {
    GenericIdent.new(i, g)
  }
  rule(void: simple(:v)) { v.to_s }
  rule(ptr: simple(:p)) { :ptr }
  rule(ref: simple(:r)) { :ref }

  rule(stmts: sequence(:s)) {
    StmtList.new s
  }
  rule(stmts: simple(:s)) {
    StmtList.new Array.wrap s
  }
  rule(bracket_stmts: sequence(:s)) {
    BracketStmts.new s
  }
  rule(wchar: simple(:w), string: simple(:s)) {
    StringLit.new(w, s)
  }
  rule(type: {derived: subtree(:d), base: subtree(:b), storage: simple(:s)}){
    Type.new(b, Array.wrap(d), s)
  }
  rule(type: {derived: subtree(:d), base: subtree(:b)}) {
    Type.new b, Array.wrap(d), nil
  }

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
  rule(ident_def: {
    names: sequence(:n), type: simple(:t)}) {
    IdentDef.new n, t, nil
  }
  rule(ident_def: {
    names: sequence(:n), type: simple(:t), default: simple(:d)}) {
    IdentDef.new n, t, d
  }
  rule(ident_def: {
    names: simple(:n), type: simple(:t) }) {
    IdentDef.new n, t, nil
  }
  rule(ident_def: {
    names: simple(:n), type: simple(:t),
    default: simple(:d)  }) {
    IdentDef.new n, t, d
  }
  # rule(var_decl: {
  #   name: simple(:n), type: simple(:t),
  #   })
  rule(var_decl: {name: simple(:n), type: simple(:t)}) {
    VarDeclSimple.new(n, t, nil)
  }
  rule(var_decl: {name: simple(:n), type: simple(:t), expr: simple(:x)}){
    VarDeclSimple.new(n, t, x)
  }
  rule(var_decl: {names: subtree(:n), type: simple(:t)}) {
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
  rule(ctor_decl: subtree(:c)) {
    CtorDecl.new(c[:args], c[:initializers], c[:body], c[:name])
  }
  rule(dtor_decl: subtree(:d)) {
    DtorDecl.new(d[:body], d[:specifier] || nil)
  }
  rule(parens: simple(:x)) { 
    ParenExpr.new x
  }
  rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
    ternary: simple(:t), true: simple(:xT), false: simple(:xF)) {
    TernaryExpr.new(Expr.new(p,x,pp), xT, xF)
  }
  rule(parens: simple(:x), args: simple(:a)) {
    FuncCall.new ParenExpr.new(x), a
  }
  rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp)) {
    Expr.new(p, x, pp)
  }
  rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp), 
    args: subtree(:a)) {
    FuncCall.new(Expr.new(p, x, nil), a)
  }
  rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
    generics: subtree(:g), args: subtree(:a)) {
    FuncCall.new(Expr.new(p, x, pp), a, g)
  }
  rule(prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
    args: subtree(:a),    infix: simple(:i), right: simple(:x2)) {
    InfixExpr.new FuncCall.new(Expr.new(p, x, nil), a), i.to_s.intern, x2
  }
  rule(expr: simple(:x), args: subtree(:a), infix: simple(:i), right: simple(:r)) {
    InfixExpr.new FuncCall.new(x, a), i.to_s.intern, r
  }
  # rule(prefix: simple(:p), expr: simple(:x), args: simple(:a)) {
  #   FuncCall.new(Expr.new(p.to_s, x, nil), a)
  # }
  rule(
    prefix: simple(:p), expr: simple(:x),
    infix: simple(:i), right: simple(:x2)) {
    InfixExpr.new(Expr.new(p, x, nil), i.to_s, x2)
  }
  rule(
    prefix: simple(:p), expr: simple(:x), postfix: simple(:pp),
    infix: simple(:i), right: simple(:x2)) {
    InfixExpr.new(Expr.new(p, x, pp), i.to_s, x2)
  }
  rule(
    expr: simple(:x), postfix: simple(:pp),
    infix: simple(:i), right: simple(:x2)) {
    InfixExpr.new(Expr.new(nil, x, pp), i.to_s, x2)
  }
  rule(expr: simple(:x)) {
    Expr.new(nil, x, nil)
  }
  rule(expr: simple(:x), postfix: simple(:pp)) {
    Expr.new(nil, x, pp)
  }
  rule(expr: simple(:l), infix: simple(:i), right: simple(:r)) {
    InfixExpr.new(l, i.to_s, r)
  }
  rule(expr_stmt: simple(:x)) {
    ExprStmt.new(x)
  }
  rule(expr: simple(:x), args: subtree(:a)) {
    FuncCall.new x, a
  }
  rule(parens: simple(:x), args: subtree(:a)) {
    FuncCall.new x, a
  }
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
  rule(conditional: {
    kind: 'for',
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