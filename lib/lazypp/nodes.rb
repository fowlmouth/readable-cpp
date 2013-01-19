class Array
  def self.wrap x
    x.is_a?(Array) ? x :
    x.is_a?(Hash) ? [x] : [*x]
  end
end

# class Object
#   private
#   def CallerInspect c
#     c = c.map  { |x|
#       (x =~ /(\/[.\w\/]+):(\d+):in `([^']+)'/) ? 
#       [$1,$2.to_i, $3] :
#       nil
#     }.compact
#     files = {}
#     c.map { |i| i[0] }.uniq.each { |file|
#       warn "reading #{file}"
#       files[file] = File.readlines file
#     }
#     res = c.uniq
#     weights = Hash[res.map { |x| [x, c.count(x)] }]
#     res.map! { |r|#(f, n, b)|
#       [*r, weights[r], files[r[0]][r[1]],
#         (r[1] .. [0, r[1]-20].max).map { |n| 
#           if files[r[0]][n] =~ /def[\s]+(([A-Za-z_][A-Za-z_0-9]*[\=!?]?)|([+\-*\/<>\[\]@]+))[\s\(]*/
#             binding.pry
#             $1
#           else
#             nil
#           end
#         }.compact
#       ]
#     }
#     binding.pry
#     res
#   end
# end
module LazyPP
class Node
  def self.new(*fields, &b)
    cls = Class.new(self)
    fff = fields.join ?,
    cls.class_eval <<-END
      attr_accessor #{fields.map {|f| ":#{f}"}.join ', '}
      def initialize(#{fff})
        #{fields.each_with_index.map { |field, index|
            "self.#{field} = #{field}"
          }.join ?;}
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
  def to_cpp(*)
    "//(Unimplemented to_cpp) #{inspect}"
  end
  def to_hpp(*args)
    to_cpp(*args)
  end
  def scan(prog)
    nil
  end
  def inspect
    "<#{self.class} #{
      instance_variables.map{|m|
        "#{m}=#{instance_variable_get(m).inspect}"
      }.join(', ')
    }>"
  end
end
class RenderState
  attr_reader :opts
  def initialize opts={}
    @opts = { 
      indent: 0, parent: nil, program: nil, namespace: []
    }.merge(opts.is_a?(RenderState) ? opts.opts : opts)
  end
  def new opts={}
    RenderState.new(@opts.merge opts)
  end
  def gen_header() @opts[:gen_header] end
  def gen_header?() !!@opts[:gen_header] end
  def parent=(p)
    @opts[:parent]=p
  end
  def parent() @opts[:parent] end
  def program() @opts[:program] end
  def indent(by=1) 
    RenderState.new(@opts.merge(indent: @opts[:indent]+by))
  end
  def indentation() '  '*@opts[:indent]  end
end

Conditional = Node.new(:kind, :condition, :body) do
  def scan *args; body.scan(*args) unless body.nil? end
  def kind= val
    val = val.to_s
    @kind = if val =~ /elseif/i; "else if" #               ;)
    else val end
  end

  def to_cpp(rs)
    "#{rs.indentation}#{kind.to_cpp}#{
      "(#{condition.to_cpp rs})" unless condition.nil?
    } {\n#{body.to_cpp(rs.indent)}\n#{rs.indentation}}"
  end
end
RangeForStmt = Node.new(:var, :container, :body) do
  def scan p; p.set_cpp0x; body.scan p end
  def to_cpp rs
    "#{rs.indentation}for(#{var.to_cpp(rs)} : #{container.to_cpp rs}) {\n"+
    body.to_cpp(rs.indent)+
    "#{rs.indentation}}"
  end
end
ForStmt = Class.new(Conditional) do
  def scan *args; body.scan(*args); condition.each{|c|c.scan(*args)} end
  def to_cpp rs
    "#{rs.indentation}for(#{last = condition[0].to_cpp(rs)}#{
      ';' unless last[-1] == ';'}#{condition[1].to_cpp rs};#{condition[2].to_cpp rs}) {\n#{
      body.to_cpp(rs.indent)}\n#{rs.indentation}}"
  end
end
NamespaceIdent = Node.new(:names) do
  def names= val; @names = Array.wrap(val) end
  def to_cpp(rs) names.map{|n|n.to_cpp(rs)}.join '::' end
end


DotName = Node.new(:name) do
  def name= val; @name = Array.wrap(val) end
  def to_cpp(*)
    if caller.size > 500
      c=CallerInspect(caller)
      #binding.pry
    end
    name.map { |n| 
      n.is_a?(Hash) ? n.values : n 
    }.flatten(1).map(&:to_cpp).join
  end
  def prefix name; @name.insert 0, name end
end
Visibility = Node.new(:v) do
  def to_cpp rs = RenderState.new();   end
  def to_hpp rs=RenderState.new
    raise "visiblity stmt only allowed in a class decl" \
      unless rs.parent.is_a? ClassDecl
    "#{v}:"
  end
end
FuncCall = Class.new Node.new(:name, :args, :generics) do
  def initialize(n, a, g = nil)
    super n, a, g
  end
  def args= val
    @args = if val == ?! || val == '()'
      nil
    else
      Array.wrap val
    end
  end
  def generics= val
    @generics = val.nil? ? nil : Array.wrap(val)
  end
  def to_cpp(rs)
    name.to_cpp(rs) + "#{"<#{generics.map { |g| g.to_cpp(rs) % ''}.join', '}>" unless generics.nil?}(" + (
      args.nil? ? '' : args.map{|a|a.to_cpp rs}.join(', ')) + ')'

      #args != ?! && [*args].map(&:to_cpp).join(', ') || '') << ')'
  end
end
ExprStmt = Node.new(:expr) do
  def to_cpp(rs); rs.indentation + expr.to_cpp + ';' end
end
LangSection = Node.new(:txt) 
AutoDecl = Node.new(:name, :val) do
  def scan p
    p.set_cpp0x
  end
  def to_cpp(rs)
    "#{rs.indentation}auto #{name.to_cpp rs} = #{val.to_cpp rs};"
  end
end
ReturnStmt = Node.new(:expr) do
  def to_cpp rs
    "#{rs.indentation}return #{expr.to_cpp rs};"
  end
end
StmtList = Class.new(Array) do #Node.new(:stmts) do 
  #def [](key) stmts[key] end 
  def inspect
    "<#{self.class}(#{size}) #{map { |s| s.inspect }.join(', ') }>"
  end
  def scan p
    each { |s| s.scan p }
  end
  def to_hpp(rs = RenderState.new)
    map { |s| s.to_hpp(rs) }.join"\n"
  end
  def to_cpp(rs = RenderState.new, &b)
    #stmts.
    #map(&:to_cpp).join("\n")
    if block_given?
      select(&b).map { |s| s.to_cpp(rs, &b) }.join"\n"
    else
      map { |s| s.to_cpp(rs) }.join"\n"
    end
  end
  def at index; self[index] end
end
class BracketStmts < StmtList
  def to_cpp rs, &b
    "#{rs.indentation}{\n"+
    super(rs.indent, &b)+
    "#{rs.indentation}}"
  end
end
Type = Node.new(:base, :derived, :storage) do
  attr_accessor :constness, :static, :inline
  attr_accessor :specifiers
  def initialize(b, d, s)
    self.base = b
    self.derived = d
    self.specifiers =Set.new
    self.storage = s
  end
  def derived= val
    @derived = (val == [''] || val.nil?) ? nil : Array.wrap(val) 
  end
  def base= val; @base = Array.wrap val; end
  def derived_cpp(rs, m = :to_cpp)
    #@derived_cpp ||= ( ##caching bad, this can change between header/src
      res = '%s'
      left = false
      derived.each do |d|
        d = Hash[d] if d.is_a?(Array)
        next if d.empty?

        # if d.has_key? :constness; self.constness = d[:constness].to_s
        # #elsif d.has_key? :static; self.static = d[:static].to_s

        case d 
        when :ptr
          res = "*#{res}"
          left = true
        when :ref
          res = "&#{res}"
          left = true
        when Hash
          #if d.has_key?(:size) || d.has_key?(:array)
          if d.has_key?(:array)
            n = "[#{d[:array] && d[:array].send(m, rs)}]"
            #n = "[#{d[:size] && d[:size].send(m, rs)}]"
            res = if left; left = false; "(#{res})#{n}"
                  else; "#{res}#{n}" end
          elsif d.has_key? :args
            n = "(#{[*d[:args]].map { |a| a.send(m, rs) % '' }.join', '})"
            res = if left; left = false; "(#{res})#{n}"
                  else; "#{res}#{n}" end
          
          elsif d.has_key? :const
            res = "#{d[:const].to_s.downcase} #{res}"
            left = true
          # elsif d.has_key? :storage
          #   @storage = d[:storage].to_s.downcase unless d[:storage].nil?
          else
            ##what the hell is this? :o
            binding.pry
          end
        else
          binding.pry
        end
      end unless derived.nil?
      res
    #)
  end
  def storage_cpp() "#{"#{storage.to_s.downcase} " unless storage.nil?}" end
  def base_hpp rs
    derived_cpp rs
    storage_cpp + base.map{|x|x.to_hpp rs}.join(' ')
  end
  def base_cpp rs
    derived_cpp rs #just to cache it
    storage_cpp + base.map{|x|x.to_cpp rs}.join(' ')
  end
  def to_hpp rs
    base_hpp(rs) << ' ' << derived_cpp(rs)
  end
  def to_cpp rs ##note to self: % format result with the name
    base_cpp(rs) << ' ' << derived_cpp(rs)
  end
end
TypeDecl = Node.new(:name, :type) do
  def to_cpp rs
    rs.gen_header? ? '' : to_hpp(rs)
  end
  def to_hpp rs
    binding.pry if \
      type.derived.nil? && (type.base[0].is_a?(UnionType))# || type.base[0].is_a?(EnumType))
    "#{rs.indentation}typedef #{type.base_cpp rs} #{type.derived_cpp(rs) % name.to_cpp(rs)};"
  end
end
class ClassDecl < TypeDecl
  attr_accessor :body, :parents
  def initialize(n,t,b,p)
    self.name=n;self.type=t;self.body=b;self.parents=p
  end

  def scan p; body.scan(p) unless body.nil? end
  def type= val
    @type = (val.to_s.downcase.intern rescue :class)
  end
  def parents= val
    @parents = val.nil? ? val : Array.wrap(val)
  end
  def to_cpp(rs = RenderState.new())
    rs = rs.new(parent: self)
    "#{to_hpp(rs)+"\n" unless rs.gen_header?}" +
    (body.nil? ? '' : body.to_cpp(rs){|n|
      if n.is_a?(VarDeclInitializer) || n.is_a?(VarDeclSimple) ||
         n.is_a?(TypeDecl)
        false
      else
        n
      end } )
  end
  def to_hpp(rs)
    rs = rs.new parent: self
    "#{type} #{name} #{
      ": #{
        parents.map { |p| 
          "#{p[:vis].to_cpp(rs)} #{p[:parent].to_cpp}"
        }.join', '} \n" unless parents.nil?}" +
      (body.nil? ? ';' : "{\n#{
        body.to_hpp(rs.indent) unless body.nil?
      }\n"\
      "};")
  end
end
Expr = Node.new(:prefix, :expr, :postfix) do
  def to_cpp(rs)
    # prefix.nil? && postfix.nil? ? 
    # expr.to_cpp : 
    # "(#{prefix.to_cpp}#{expr.to_cpp}#{postfix.to_cpp})"
    "#{prefix.to_cpp rs}#{expr.to_cpp rs}#{postfix.to_cpp rs}"
  end
end
DotExpr = Node.new(:exprs) do
  def to_cpp(rs = RenderState.new) 
    "#{exprs.map{|e|
      e.to_cpp rs
    }.join''}" 
  end
end
AccessExpr = Node.new(:access, :expr) do
  def to_cpp(rs) "#{access.to_cpp rs}#{expr.to_cpp rs}" end
end
ParenExpr = Node.new(:expr) do
  def to_cpp(rs) "(#{expr.to_cpp rs})" end
end
CastExpr = Node.new(:type, :expr) do
  def to_cpp rs
    "(#{type.to_cpp(rs) % ''})(#{expr.to_cpp rs})"
  end
end
CaseStmt = Node.new(:exprs, :body) do
  def scan(*args) body.each { |b| b.scan *args } end
  def to_cpp(rs)
    i = rs.indentation
    (exprs == :default ? 
      i+"default:\n" : 
      exprs.map { |x| i+"case #{x.to_cpp rs}:\n"}.join('')
    ) + 
    body.to_cpp(rs.indent) +
    "\n#{rs.indent.indentation}break;"
  end
end
SwitchStmt = Node.new(:expr, :body) do
  def scan(*args) body.each { |b| b.scan *args } end
  def to_cpp(rs)
    "#{rs.indentation}switch(#{expr.to_cpp rs}) {\n" +
    body.map { |s| s.to_cpp(rs) }.join("\n") +
    "\n#{rs.indentation}}"
  end
end
InfixExpr = Node.new(:left, :infix, :right) do
  def to_cpp rs
    #binding.pry
    "#{left.to_cpp rs
    } #{infix.to_cpp rs
    } #{right.to_cpp rs}"
  end
end
TernaryExpr = Node.new(:cond, :t, :f) do
  def to_cpp rs
    "#{cond.to_cpp rs} ? #{t.to_cpp rs} : #{f.to_cpp rs}"
  end
end
BracketExpr = Node.new(:expr) do
  def to_cpp(rs=RenderState.new)
    "[#{expr.to_cpp(rs)}]"
  end
end
GenericIdent = Node.new(:ident, :generic) do
  def generic= val
    @generic = Array.wrap(val)
  end
  def to_cpp(rs)
    "#{ident.to_cpp(rs)}<#{generic.map{|n| n.to_cpp(rs) % ''}.join', '}>"
  end
end
UnionType = Node.new(:members)
EnumType = Node.new(:fields) do
  def to_cpp(rs)
    "enum{#{fields.map{|f|f.to_cpp rs}.join', '}}"
  end
end
StructType = Node.new(:body) do
  def to_cpp(rs)
    "struct{#{body.to_cpp(rs)}}"
  end
end
VarDeclSimple = Node.new(:name, :type, :val) do
  def to_cpp(rs = RenderState.new())
    # res = type.to_cpp
    # [*names].map { |n| (res + ';') % n }.join('')
    rs.indentation + (type.base_cpp(rs) % '') + ' ' + (type.derived_cpp(rs)%name) + 
      (val && " = #{val.to_cpp(rs)};" || ';')
  end
end
VarDeclInitializer = Node.new(:names, :type)do
  def to_cpp(rs = RenderState.new())
    derived = type.derived_cpp rs #cpp
    rs.indentation + type.base_cpp(rs) + ' ' + [*names].map { |n| 
      name = derived % n.name rescue binding.pry
      if n.args.nil?
        name
      else
        name + '(' + [*n.args].map{|a|a.to_cpp(rs)}.join(', ') + ')'
      end
    }.join(', ') + ';'
  end
end 
ConstructorName = Node.new(:name, :args)
IdentDef = Node.new(:names, :type, :default) do
  def names= val; @names = Array.wrap(val) end
  def to_cpp(rs)
    #return to_hpp(rs)  unless rs.gen_header
    t = type.to_cpp(rs)
    names.map { |n| t % n }.join', '
  end
  def to_hpp(rs)
    t = type.to_hpp(rs)
    names.map { |n|
      res = t % n
      (res << ' = ' << default.to_cpp(rs)) unless default.nil?
      res
    }.join', '
  end
end
ImportStmt = Node.new(:pkg) do
  attr_reader :p
  def initialize(pkg)
    @p = Package.new(pkg)
  end

  def scan(prog)
    prog.notify self
  end

  def to_cpp(rs)
    @p.includes.map { |i|
      "#include #{i}\n"
    }.join('') unless @p.includes.nil?
  end
end
require'forwardable'
StringLit = Node.new(:wchar, :str) do
  def to_cpp(*); "#{?L if wchar}\"#{str}\"" end
  def str= val; @str = val.to_s end
  extend Forwardable
  def_delegators :@str, :=~, :to_s
end
DefineStmt = Node.new(:name, :expr) do
  def to_cpp(rs)
    to_hpp(rs) if not rs.gen_header?
  end
  def to_hpp(rs)
    "#define #{name.to_cpp rs} #{expr.to_cpp rs}"
  end
end
StructLit = Node.new(:x) do
  def x= val; @x = Array(val) end
  def to_cpp(rs)
    "{#{x.map{|_|_.to_cpp rs}.join', '}}"
  end
end
TryStmt = Node.new(:attempt, :catches) do
  def to_cpp rs
    "#{rs.indentation}try {\n#{attempt.to_cpp rs.indent}\n"\
    "#{rs.indentation}} #{catches.map{|c|c.to_cpp rs}.join''}"
  end
end
CatchStmt = Node.new(:c, :body) do
  def to_cpp rs
    "#{rs.indentation}catch (#{c.to_cpp rs}) {\n"\
    "#{body.to_cpp rs.indent}\n"\
    "#{rs.indentation}}"
  end
end
IncludeStmt = Node.new(:file, :type) do
  def file= val
    if file && file.is_a?(StringLit)
      @file.str = val
    else
      @file = val
    end
  end
  def scan p; p.notify self end
  def to_cpp(rs)
    "#include " << (
      type == :std ? 
        '<' << file << '>' : 
        file.to_cpp)
  end
end
UsingStmt = Node.new(:name) do
  def to_cpp(rs)
    "using #{name.to_cpp rs};"
  end
end
IntLiteral = Node.new(:value) do
  def to_cpp(*); "#{value}" end
end
CharLiteral = Node.new(:char) do
  def to_cpp(*); "'#{char}'" end
end
FloatLiteral = Node.new(:value, :type) do
  def to_cpp(*)
    "#{value}#{type}"
  end
end

Namespace = Node.new(:name, :body) do
  def to_cpp(rs, &b)
    "namespace #{name} {\n" << body.to_cpp(rs,&b) << "\n}"
  end
end
DtorDecl = Node.new(:stmts, :specifier) do
  def specifier= val
    @specifier = if val.nil? then ''
    else
      val.to_s + ' '
    end
  end

  def to_cpp rs
    #parent = rs.parent
    parentname = rs.parent.name.to_cpp rs
    "#{rs.indentation}#{parentname}::~#{parentname}() \n"\
    "#{rs.indentation}{\n"\
    "#{stmts.to_cpp(rs.indent)}\n"\
    "#{rs.indentation}}"
  end
  def to_hpp rs
    "#{rs.indentation}#{
      specifier unless specifier.nil?
    }~#{
      rs.parent.name.to_hpp(rs)
    }();"
  end
end
CtorDecl = Node.new(:args, :initializers, :body, :name) do
  def args= a; @args = Array.wrap a; end
  def initializers= i; @initializers = (i.nil? ? nil : Array.wrap( i)); end
  def scan(*args) body.scan(*args) end
  def to_cpp rs
    parent = rs.parent
    "#{rs.indentation}#{parent.name}::#{name.nil? ?
      parent.respond_to?(:name) ? 
        parent.name : 
        '/*constructor name missing and parent node has no name*/' :
      name
    }(#{args.map{|a|a.to_cpp rs}.join', '}) #{
      ( ': ' +
        initializers.map{|i|
          i = i[:initializer]
          "#{i[:member].to_cpp rs}(#{[*i[:args]].map{|a|a.to_cpp rs}.join(', ')})"
        }.join(', ')
      ) unless initializers.nil? 
    } #{body.nil? ? ?; : "{\n#{body.to_cpp(rs.indent)}\n#{rs.indentation}}"}"
  end
  def to_hpp rs
    parent = rs.parent
    "#{rs.indentation}#{name.nil? ?
      parent.respond_to?(:name) ?
        parent.name.to_hpp(rs) :
        '/*constructor name missing and parent node has no name*/' :
      name
    }(#{args.map{|a|a.to_hpp rs}.join', '});"
  end
end
FuncDecl = Class.new Node.new(:name, :sig, :body, :generics, :specifiers) do
  def initialize(n,s,b,g,ss= nil)
    super
  end
  def specifiers= val; @specifiers = val.nil? ? nil : Array.wrap(val) end
  def generics= val; @generics = val.nil? ? nil : Array.wrap(val) end
  def scan(*args) body.scan(*args) unless body.nil? end
  def to_cpp(rs)
    ## more info on functions returning functions and such at http://www.newty.de/fpt/fpt.html
    "#{"#{rs.indentation}template <#{generics.map{|x|'typename '+ x.to_cpp(rs)}.join', '}>\n" if generics}" +
    "#{rs.indentation}#{
      sig.base_cpp(rs)} #{
      sig.derived_cpp(rs) % "#{rs.parent && rs.parent.name.to_cpp+'::'}#{name}"}"+
    "#{body == :EQ0 ? 
      ' = 0;' :
      body.nil? ? 
        ';' : 
        "\n#{rs.indentation}{\n#{
          body.to_cpp(rs.indent) }\n"\
        "#{rs.indentation}}"
    }"
  end
  def to_hpp(rs)
    "#{rs.indentation}#{
      specifiers.map{|s|s.to_hpp rs}.join(' ')+' ' if !specifiers.nil?}#{
    sig.base_hpp rs} #{sig.derived_cpp(rs, :to_hpp) % name};"
  end
end
class OperDecl < FuncDecl
  attr_accessor :pos
  def initialize n,s,b,p
    self.name = n
    self.body = b
    self.pos = p
    self.sig = s
  end
  def name= val
    @name=val.to_s
    @name=:implicit if name =~ /implicit/i
  end
  def sig= val
    if pos == "post" && val.derived[0][:args] == 'void'
      #dont remember this. what is it for?
      val.derived[0][:args] = Type.new(:int, nil)
      binding.pry
    end
    @sig = val
  end

  def decl_header rs, qualify
    n = 'operator ' + if name == :implicit
      sig.base_cpp(rs)+(sig.derived_cpp(rs)%'')
    else
      name.to_cpp(rs)
    end
    n.prepend rs.parent.name.to_cpp(rs)+'::' if qualify && !rs.parent.nil?
    
    rs.indentation +
    (name == :implicit ?
      n :
      sig.base_cpp(rs) + ' ' + (sig.derived_cpp(rs) % n))
  end
  def to_cpp rs
    decl_header(rs, true) + (body.nil? ? ?; : "\n#{rs.indentation}{\n#{body.to_cpp rs.indent}\n#{rs.indentation}}")
  end
  def to_hpp rs
    decl_header(rs, false) + ?;
  end
end
end

class Parslet::Slice; def to_cpp(*); to_s end; alias to_hpp to_cpp end
class String; def to_cpp(*); self end; alias to_hpp to_cpp end 
class Symbol; def to_cpp(*); to_s end; alias to_hpp to_cpp end
class NilClass; def to_cpp(*); '' end; alias to_hpp to_cpp end
class Hash; 
  def to_cpp(*)
    "/* Untransformed hash #{ai multiline: true, plain: true} */" 
  end
  alias to_hpp to_cpp 
  def scan(*) end
end
