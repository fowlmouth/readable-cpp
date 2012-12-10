module LazyPP

class Parser < Parslet::Parser
  include ParserMethods
  def brackets(rule) 
    l_bracket >> space? >> rule >> space? >> r_bracket
  end
  def spaced?(rule)
    space? >> rule >> space?
  end
  def parens_oder_spacen(rule)
    l_paren >> space? >> rule >> space? >> r_paren |
    space >> rule
  end

  rule(:space) { 
    (match['\s'].repeat(1) | comments).repeat(1)
  }
  rule(:space?) { space.maybe }
  rule(:comments) {
    str('/*') >> (str('*/').absent? >> any).repeat >> str('*/') |
    str('//') >> (eol.absent? >> any).repeat >> eol
  }
  rule(:string) {
    str(?L).maybe.as(:wchar) >>
    str(?") >>
    (str(?\\) >> any | str(?").absent? >> any).repeat.as(:string) >>
    str(?")
  }
  rule(:char) {
    str(?') >>
    (
      str(?\\) >> (
        str(?x) >> match['\d'].repeat(2,2) | 
        match['\d'].repeat(2,2)  |
        any
      ) | any
    ).as(:char) >> 
    str(?')
  }
  rule(:colon_is) { 
    space? >> colon >> space? |
    space  >>  `is` >> space
  }
  rule(:colon_is?) {
    colon_is | space?
  }

  rule(:ident) { (match['A-Za-z_'] >> match['A-Za-z0-9_'].repeat).as(:ident) }
  rule(:namespaced_ident) { join(ident, str('::'), 1).as(:namespace) }
  rule(:generic?) {
    (str(?<) >> comma_list(ident).as(:generics) >> str(?>)).maybe
  }
  rule(:digit) do match['\d'] end
  rule(:int) { digit.repeat(1).as(:int) }
  rule(:float) {
    (
      (digit.repeat(1) >> str(?.) >> digit.repeat).as(:float) |
      (digit.repeat >> str(?.) >> digit.repeat(1)).as(:float) 
    ) >> (`f` | str('L')).maybe.as(:type)
  }

  rule(:derived_type) {
    ((`const` | `mutable`).as(:constness) >> space).maybe >>
    ((
      str('^').as(:pointer) |
      str('[]').as(:array)  |
      str('&').as(:ref)     |
      str(?[) >> expr.as(:size) >> str(?]) |
      func_sig_args_anon >> space? >> str('->')
    ) >> space?).repeat
  }
  rule(:type) {
    (
      derived_type.as(:derived).maybe >> space? >>
      ( union_decl.as(:union) | 
        enum_decl.as(:enum)   |
        join(
          (ident | namespaced_ident) >> generic?, 
          str('::'), 0).as(:namespaced) | 
        join(ident, space, 0)
      ).as(:base)
    ).as(:type)
  }
  rule(:lang_section) {
    (
      `lang` >> space >> (
        `c` >> str("++").absent? >> space? >>
        brackets(CP.as(:text))
      )
    ).as(:lang_section)
  }
  rule(:auto_decl) {
    (
      `auto` >> space >>
      ident.as(:name) >> spaced?(str(?=)) >>
      expr.as(:value) >> space? >> semicolon
    ).as(:auto_decl)
  }
  rule(:var_decl) {
    (
      `var` >> space >> 
      ##single assign
      ( ident.as(:name) >> colon_is  >>
        type.as(:type) >> spaced?(str(?=) >> space? >> expr).maybe |
      ##multiple decl/initialize
        comma_list(ident.as(:name) >> parens(comma_list(expr).maybe).maybe.as(:constructor)).as(:names) >>
        colon_is >> type.as(:type)
      ) >>
      space? >> semicolon
      #comma_list(ident.as(:name) >> parens(comma_list(expr).maybe).as(:constructor).maybe).as(:names) >> 
      #colon_is? >> type.as(:type) >> space? >> semicolon
    ).as(:var_decl)
  }
  rule(:type_decl) {
    (
      `type` >> space >> (ident >> generic?).as(:name) >>
      colon_is >>
      (class_decl | type).as(:type) >>
      (space? >> str(';') | eol)
    ).as(:type_decl)
  }
  rule(:import_stmt) {
    `import` >> space >> join(ident, str(?/)).as(:import_pkg)
  }
  rule(:include_stmt) {
    (
      `include` >> space >>
      ( str(?<).as(:std) >> ((alphnum|str(?/)).repeat(1) >> (str('.') >> 
        alphnum.repeat(1,3)).maybe).as(:file) >> str(?>) |
        string.as(:local) 
      ) >>
      (eol | space? >> semicolon)
    ).as(:include_stmt)
  }
  rule(:alphnum) { match['A-Za-z0-9_'] }
  rule(:using_stmt) {
    (
      `using` >> space >> (`namespace`.as(:ns) >> space).maybe >>
      ident.as(:using_namespace) >>
      (eol | space? >> semicolon)
    ).as(:using_stmt)
  }
  rule(:namespace_decl) {
    (`namespace` >> space >> ident.as(:namespace) >> space? >>
    body.as(:body)).as(:namespace_decl)
  }
  rule(:return_stmt) {
    
      (`return` >> (space >> expr | parens(expr))).as(:return) >> space? >> semicolon
    
  }
  rule(:ctor_decl) {
    (
      `ctor` >> colon_is? >>
      parens(`void` | ident_defs).as(:args) >> space? >>
      join(
        ident.as(:member) >> parens(expr.as(:initial_value)),
        space? >> (comma >> space?).maybe
      ).maybe.as(:initializers) >> space? >>
      body.as(:body)
    ).as(:ctor_decl)
  }
  rule(:dtor_decl) {
    `dtor` >> space? >> (colon >> space?).maybe >>
    body
  }
  rule(:func_decl) {
    (
      `func` >> space >> ident.as(:name) >> colon_is? >>
      func_sig.as(:sig) >> space? >> body.as(:body)
    ).as(:func_decl)
  }
  rule(:oper_decl) {
    (
      `oper` >> space >> 
      ((`pre` | `post`).as(:pos) >> space).maybe >>
      operator.as(:name) >> colon_is? >>
      func_sig.as(:sig) >> space? >> body.as(:body)
    ).as(:oper_decl)
  }

  rule(:operator) {
    str('&&') | str('||') | str('==') | str('!=') |
    str('<<=')| str('>>=')| str('<=') | str('>=') |
    str('+=') | str('-=') | str('/=') | str('*=') |
    str('|=') | str('&=') |
    str('++') | str('--') | str('<<') | str('>>') |
    str('+')  | str('-')  | str('/')  | str('*')  |
    str('&')  | str('|')  | str('=')  |
    str('<')  | str('>')
  }
  rule(:operator_prefix) {
    str('++') | str('--') | match['!~+\-&*'] |
    `new` | `delete`
  }
  rule(:operator_postfix) {
    str('++') | str('--')
    #str('[') >> 
  }
  rule(:btest) {
    brackets(expr)
  }

  rule(:union_decl) {
    `union` >> space? >> brackets(ident_defs.as(:members))
  }
  rule(:enum_decl) {
    `enum` >> space? >> 
    brackets(
      join(ident_eq, space? >> (comma | semicolon) >> space?).as(:fields)
    )
  }
  rule(:ident_eq) {
    ident >> (space? >> str(?=) >> space? >> expr).maybe
  }
  rule(:class_decl) {
    (`class` | `struct`).as(:class_type) >> (
      (space? >> str(?<) >> space? | space >> `inherits` >> space) >>
      join(parent_class, space? >> comma >> space?).as(:parents)
    ).maybe >>
    space? >> brackets(program).as(:body)
  }
  rule(:parent_class) {
    (visibility.as(:vis) >> space).maybe >>
    ident.as(:parent)
  }
  rule(:visibility) {
    `public` | `private` | `protected`
  }

  rule(:func_sig) {
    func_sig_args >> space? >> 
    str('->') >> space? >> type.as(:returns)
  }
  rule(:func_sig_args_anon) {
    parens(
      `void` | 
      `any`  | 
      comma_list(type) 
    ).as(:args)
  }
  rule(:func_sig_args) {
    parens(
      `void` |
      ident_defs
    ).as(:args)
  }

  rule(:eol) {
    match[' \t'].repeat >> match['\n']
  }

  rule(:dot_ident) {
    join(ident, (str('.') | str('->')).as(:access)).as(:dot_ident)
  }
  rule(:func_call) {
    (namespaced_ident | dot_ident).as(:func) >>
    (
      str(?!) |
      parens(comma_list(expr).maybe) |
      space_nonterminal >> comma_list(expr)
    ).as(:args)
  }
  rule(:space_nonterminal) {
    match[' \t'].repeat(1) >> (str(?\\) >> space).maybe |
    str(?\\) >> space
  }

  rule(:conditional) {
    (
      (`if`|`elseif`|`while`).as(:kind) >> 
        (space >> expr | space? >> parens(expr)).as(:cond) >>
        space? >> (body | stmt).as(:body) |
      `else`.as(:kind) >> (space? >> body | space >> stmt).as(:body) |
      `for`.as(:kind) >> parens_oder_spacen(
        (var_decl | expr >> space? >> semicolon).as(:l) >>
        space? >> expr.as(:m) >> space? >> semicolon >>
        space? >> expr.as(:r) )  >>
        (space? >> body | space >> stmt).as(:body)
    ).as(:conditional)
  }

  rule(:ident_def) {
    (comma_list(ident).as(:names) >> space? >> colon >>
    space? >> type.as(:type) >>
    (space? >> str(?=) >> space? >> expr.as(:default)).maybe
    ).as(:ident_def)
  }
  rule(:ident_defs) {
    join(ident_def, spaced?(comma|semicolon))
  }
  rule(:expr) {
      (
        (operator_prefix.as(:op) >> space?).as(:prefix).maybe >>
        (
          float | int | string | char | cast | 
          func_call | namespaced_ident | dot_ident
        ).as(:expr) >>
        (operator_postfix | sq_bracket_expr).as(:op).as(:postfix).maybe |
        parens(expr)
      ) >> 
      (space? >> operator.as(:infix) >> space? >> expr.as(:right)).maybe
    
  }
  rule(:sq_bracket_expr) {
    (
      str(?[) >> space? >> expr >> space? >> str(?])
    ).as(:sq_bracket_expr)
  }
  rule(:cast) {
    `cast` >> str(?<) >> type.as(:type) >> str(?>) >> 
    parens(expr)
  }

  rule(:stmt) {
    var_decl | auto_decl | import_stmt | using_stmt | func_decl | 
    return_stmt |
    namespace_decl | type_decl | ctor_decl | dtor_decl | oper_decl |
    conditional | lang_section | include_stmt |
    (expr.as(:expr_stmt) >> space? >> semicolon)
  }
  rule(:body) {
    brackets(program.maybe)
  }
  rule(:program) {
    space? >>
    (stmt >> space?).repeat.as(:stmts)
  }

  root :program
end
end