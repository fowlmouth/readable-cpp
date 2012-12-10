module LazyPP

Transform = Parslet::Transform.new {
  rule(ident: simple(:i)) { i.to_s.intern }
  rule(ident: simple(:i), generics: simple(:g)) {
    GenericIdent.new(i, g)
  }

  rule(stmts: sequence(:s)) {
    StmtList.new s
  }
  rule(wchar: simple(:w), string: simple(:s)) {
    StringLit.new(w, s)
  }
  rule(type: {derived: subtree(:d), base: subtree(:b)}){
    Type.new(b, [*d])
  }
  rule(union: { members: subtree(:m) }) {
    UnionType.new(m)
  }
  rule(auto_decl: {name: simple(:n), value: subtree(:v)}) {
    AutoDecl.new(n, v)
  }
  rule(enum: { fields: subtree(:f) }) {
    EnumType.new f
  }
  rule(lang_section: subtree(:l)) {
    binding.pry
    LangSection.new(l)
  }
  rule(ident_def: subtree(:i)){
    IdentDef.new(i[:name], i[:type], i[:default])
  }
  rule(var_decl: {name: simple(:n), type: simple(:t)}) {
    VarDeclSimple.new(n, t, nil)
  }
  rule(var_decl: {name: simple(:n), type: simple(:t), expr: simple(:x)}){
    VarDeclSimple.new(n, t, x)
  }
  rule(var_decl: {names: subtree(:n), type: simple(:t)}) {
    VarDeclInitializer.new n, t
  }
  rule(name: simple(:n), constructor: subtree(:c)) {
    ConstructorName.new n, c
  }
  rule(import_pkg: subtree(:i)) {
    ImportStmt.new(i)
  }
  rule(include_stmt: { std: simple(:x), file: simple(:f) }) {
    IncludeStmt.new f, :std
  }
  rule(include_stmt: { local: simple(:f) }) {
    IncludeStmt.new f, :local
  }
  rule(using_stmt: { ns: simple(:ns), using_namespace: simple(:n) }) {
    UsingStmt.new("namespace #{n}")
  }
  rule(using_stmt: { using_namespace: simple(:n) }) {
    UsingStmt.new(n.to_s)
  }
  rule(namespace_decl: {namespace: subtree(:n), body: subtree(:b)}){
    Namespace.new(n, b)
  }
  # rule(oper_decl: subtree(:o)) {
  #   OperDecl.new(o[:name], o[:sig], o[:body])
  # }
  # rule(func_decl: subtree(:f)) {
  #   FuncDecl.new(f[:name], f[:sig], f[:body])
  # }
  rule(oper_decl: {
    name: simple(:name),
    sig: { args: subtree(:args), returns: subtree(:returns) },
    body: simple(:b)  }) {
    OperDecl.new name, [args, returns], b
  }
  rule(func_decl: {
    name: simple(:name),
    sig: { args: subtree(:args), returns: subtree(:returns) },
    body: simple(:b) }) {
    FuncDecl.new name, args, returns, b
  }
  rule(op: simple(:o)) { o.is_a?(Parslet::Slice) ? o.to_s.intern : o }
  rule(float: simple(:f), type: simple(:t)) {
    FloatLiteral.new(f, t)
  }
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
    CtorDecl.new(c[:args], c[:initializers], c[:body])
  }
  rule(prefix: simple(:p), expr: simple(:x)) {
    Expr.new(p.to_s, x, nil)
  }
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
  rule(func: simple(:name), args: subtree(:args)) {
    FuncCall.new name, args
  }
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
  rule(namespaced: simple(:i)) {
    NamespaceIdent.new i
  }
  rule(namespaced: sequence(:i)) {
    NamespaceIdent.new(i)
  }
  rule(sq_bracket_expr: simple(:x)) {
    BracketExpr.new x
  }
}
class Node
  def self.new(*fields, &b)
    cls = Class.new(self)
    fff = fields.join ?,
    cls.class_eval <<-END
      attr_accessor #{fields.map {|f| ":#{f}"}.join ', '}
      def initialize(#{fff})
        #{fields.map {|f| "@#{f}"}.join','} = #{fff}
      end
      def self.new(*args)
        o = allocate
        o.send :initialize, *args
        o
      end
    END
    cls.class_eval &b if b
    cls
  end
  def to_cpp
    "//(Unimplemented to_cpp) #{inspect}"
  end
  def inspect
    "<#{self.class} #{
      instance_variables.map{|m|
        "#{m}=#{instance_variable_get(m).inspect}"
      }.join(', ')
    }>"
  end
end
Conditional = Node.new(:kind, :condition, :body) do
  def to_cpp
    "#{kind}(#{condition.to_cpp}) {\n#{body.to_cpp}\n}"
  end
end
ForStmt = Class.new(Conditional) do
  def to_cpp
    "for(#{last = condition[0].to_cpp}#{
      ';' unless last[-1] == ';'}#{condition[1].to_cpp};#{condition[2].to_cpp}) {\n#{
      body.to_cpp}\n}"
  end
end
NamespaceIdent = Node.new(:names) do
  def to_cpp; [*names].map(&:to_cpp).join '::' end
end
DotName = Node.new(:name) do
  def to_cpp; [*name].map { |n|
    n.is_a?(Hash) ? n.values : n
  }.flatten.map(&:to_s).join end
end

FuncCall = Node.new(:name, :args) do
  def to_cpp
    name.to_cpp + '(' << (
      args != ?! && [*args].map(&:to_cpp).join(', ') || '') << ')'
  end
end
ExprStmt = Node.new(:expr) do
  def to_cpp; expr.to_cpp + ';' end
end
LangSection = Node.new(:txt) 
AutoDecl = Node.new(:name, :val) do
  def to_cpp
    "auto #{name.to_cpp} = #{val.to_cpp};"
  end
end
ReturnStmt = Node.new(:expr) do
  def to_cpp
    "return #{expr.to_cpp};"
  end
end
StmtList = Class.new(Array) do #Node.new(:stmts) do 
  #def [](key) stmts[key] end 
  def inspect
    "<#{self.class} #{map { |s| s.inspect }.join(', ') }>"
  end
  def to_cpp
    #stmts.
    map(&:to_cpp).join("\n")
  end
  def at index; self[index] end
end
Type = Node.new(:base, :derived) do
  def derived_cpp
    res = '%s'
    constness = nil
    derived.each do |d|
      #sometimes const comes through as [:constness, 'const'].. shrug
      if d.is_a?(Array) then d = Hash[*d] end
      
      if d.has_key? :constness
        constness = d[:constness].to_s
      elsif d.has_key? :pointer
        res = "(*#{res})"
      elsif d.has_key? :array
        res = "(#{res}[])"
      elsif d.has_key? :size
        res = "(#{res}[#{d[:size].to_cpp}])"
      elsif d.has_key? :args
        #binding.pry
        res = "(#{res}(#{
          [*d[:args]].map {|a| a.to_cpp % ''}.join', '
        }))"
      end
    end unless derived == '' || derived == [''] ##this happens too, somehow ._.

    unless constness.nil? 
      constness << ' ' << res
    else res end
  end
  def base_cpp
    [*base].map(&:to_cpp).join(' ')
  end
  def to_cpp ##note to self: % format result with the name
    base_cpp << ' ' << derived_cpp
  end
end
TypeDecl = Node.new(:name, :type) 
class ClassDecl < TypeDecl
  attr_accessor :body, :parents
  def initialize name, type, body, parents
    @name, @type, @body, @parents = name, type, body, parents
  end
  def to_cpp
    "class #{name} #{not parents.nil? and parents.map(&:to_cpp).join(', ')} {\n#{body.to_cpp}\n}\n"
  end
end
Expr = Node.new(:prefix, :expr, :postfix) do
  def to_cpp
    prefix.nil? && postfix.nil? ? 
    expr.to_cpp : 
    "(#{prefix.to_cpp}#{expr.to_cpp}#{postfix.to_cpp})"
  end
end
InfixExpr = Node.new(:left, :infix, :right) do
  def to_cpp
    "#{left.to_cpp} #{infix.to_cpp} #{right.to_cpp}"
  end
end
BracketExpr = Node.new(:expr) do
  def to_cpp
    "[#{expr.to_cpp}]"
  end
end
GenericIdent = Node.new(:ident, :generic) do
  def to_cpp
    "#{ident.to_cpp}<#{[*generic].map(&:to_cpp).join', '}>"
  end
end
UnionType = Node.new(:members)
EnumType = Node.new(:fields)
VarDeclSimple = Node.new(:name, :type, :val) do
  def to_cpp
    # res = type.to_cpp
    # [*names].map { |n| (res + ';') % n }.join('')
    (type.to_cpp % name) << (val && " = #{val.to_cpp};" || ';')
  end
end
VarDeclInitializer = Node.new(:names, :type)do
  def to_cpp
    derived = type.derived_cpp
    type.base_cpp + ' ' + [*names].map { |n| 
      name = derived % n.name
      if n.args.nil?
        name
      else
        name + '(' + [*n.args].map(&:to_cpp).join(', ') + ')'
      end
    }.join(', ') + ';'
  end
end 
ConstructorName = Node.new(:name, :args)
IdentDef = Node.new(:names, :type, :default) do
  def to_cpp ##test this
    t = type.to_cpp
    [*names].map { |n| 
      res = (t % n) 
      (res << ' = ' << default.to_cpp) unless default.nil?
      res
    }.join', '
  end
end
ImportStmt = Node.new(:pkg) do
  attr_reader :p
  def initialize(pkg)
    @p = Package.new(pkg)
  end

  def to_cpp 
    @p.includes.map { |i|
      "#include #{i}\n"
    }.join('')
  end
end
StringLit = Node.new(:wchar, :str) do
  def to_cpp; "#{?L if wchar}\"#{str}\"" end
end
IncludeStmt = Node.new(:file, :type) do
  def to_cpp
    "#include " << (type == :std ? 
      '<' << file.to_s << '>' : 
      file.to_cpp)
  end
end
UsingStmt = Node.new(:name) do
  def to_cpp
    "using #{name};"
  end
end
IntLiteral = Node.new(:value) do
  def to_cpp; "#{value}" end
end
CharLiteral = Node.new(:char) do
  def to_cpp; "'#{char}'" end
end
FloatLiteral = Node.new(:value, :type) do
  def to_cpp
    "#{value}#{type}"
  end
end

Namespace = Node.new(:name, :body) do
  def to_cpp
    "namespace #{name} {\n" << body.to_cpp << '}'
  end
end
CtorDecl = Node.new(:args, :initializers, :body) do
  def initialize a,i,b
    @args, @initializers, @body = [*a], i ? [*i] : nil, b
  end
end
FuncDecl = Node.new(:name, :args, :returns, :body) do
  def to_cpp
    (returns.to_cpp % name) << args.to_cpp << "{\n" << body.to_cpp <<
    "\n}"
  end
end
class OperDecl < FuncDecl; end

end


class Parslet::Slice; def to_cpp; to_s end end
class String; def to_cpp; self end end 
class Symbol; def to_cpp; to_s end end
class NilClass; def to_cpp; '' end end
class Hash; def to_cpp; "/* Untransformed hash #{self.inspect}" end end
