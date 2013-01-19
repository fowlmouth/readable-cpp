module LazyPP

class Parser < Parslet::Parser
  include ParserMethods
  def brackets(rule) 
    l_bracket >> space? >> rule >> space? >> r_bracket
  end
  def brackets?(rule)
    brackets(rule) | rule
  end
  def spaced?(rule)
    space? >> rule >> space?
  end
  def parens_oder_spacen(rule)
    l_paren >> space? >> rule >> space? >> r_paren |
    space >> rule
  end
  def parens(rule)
    l_paren >> space? >> rule >> space? >> r_paren
  end
  def parens?(rule)
    parents(rule) | rule
  end
  def paren_tagged(rule, tag=:parens)
    parens(rule.as(tag))
  end
  def paren_tagged?(rule, tag)
    paren_tagged(rule,tag) |
    rule
  end

  rule(:space) { 
    (match['\s'].repeat(1) | comment).repeat(1)
  }
  rule(:space?) { space.maybe }
  rule(:comment) {
    str('/*') >> (str('*/').absent? >> any).repeat >> str('*/') |
    str('//') >> (eol.absent? >> any).repeat >> eol
  }
  ##TODO save comments in program
  rule(:space_comments) {
    (comment.as(:comment) | match['\s'].repeat(1)).repeat(1)
  }
  rule(:space_comments?) { space_comments.maybe }
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
  #TODO another rule for just generic ident for function/class decls
  rule(:generic?) {
    (str(?<) >> comma_list(type#|namespaced_ident|ident#|type
      ).as(:generics) >> str(?>)).maybe
  }
  rule(:digit) do match['\d'] end
  rule(:int) { digit.repeat(1).as(:int) }
  rule(:float) {
    (
      (digit.repeat(1) >> str(?.) >> digit.repeat).as(:float) |
      (digit.repeat >> str(?.) >> digit.repeat(1)).as(:float) 
    ) >> (`f` | str('L')).maybe.as(:type)
  }

  # rule(:specifier) {
  #   `static`.as(:static) | `inline`.as(:inline) |
  #   `virtual`.as(:virtual) | `explicit`.as(:explicit) |
  #   #(`const` | `mutable`).as(:const)
  # }

  rule(:specifier) {
    (`inline` | `virtual` | `explicit`).as(:ident)
  }

  rule(:specifiers) {
    (
      join(specifier, space, 0)
    ).as(:specifiers)
  }
  rule(:specifiers?) { specifiers.maybe }


  rule(:ptr) { str(?^) }
  rule(:ptr_eng) { `pointer to` | `ptr to` | ptr }
  rule(:ref) { str(?&) }
  rule(:ref_eng) { `reference to` | `ref to` | ref }
  rule(:const) { `const` | `mutable` | `noalias` }
  rule(:storage) { 
    `extern` | `static` | `auto` | `register` 
  } 
  rule(:modifier) { 
    `short` | `long` | `signed` | `unsigned` | const
  }

  ## UNFINISHED
  ## need func_sig_args_eng and extra handling for :array_unsized in nodes.rb
  rule(:derived_english) {
    (storage.as(:storage) >> space).maybe >>
    join(
      const.as(:const) >> (space >> (ref_eng|ptr_eng)).present? |
      ptr_eng.as(:ptr) |
      ref_eng.as(:ref) |
      (`array of` | str('[]')).as(:array_unsized) |
      `array with ` >> (
        int | 
        #considered this to use array of for both sized/unsized
        #(ptr_eng|ref_eng|`array`|`function`|const).absnt? >> ident
        #also i considered only allowing an int here
        #both are tolerable solutions imo
        ident
      ).as(:array).maybe | 
      func_sig_args_eng >> space >> `returning`,

      space
    )
  }

  rule(:derived_type) {
    join(##if ref/ptr isnt next, hopefully you're at the end of the chain so const will be in the base type,
         ##otherwise this would be an invalid type. as a side effect, parsing will fail. <3
      const.as(:const) >> (space? >> (ref|ptr)).present? | 
      ptr.as(:ptr) |
      ref.as(:ref) |
      str(?[) >> space? >> expr.maybe.as(:array) >> space? >> str(?]) |
      func_sig_args >> space? >> str('->'),

      space?
    )
  }


  rule(:type) {
    (
      (storage.as(:storage) >> space).maybe >>
      derived_type.maybe.as(:derived) >> space? >>
      ( union_decl.as(:union) | 
        enum_decl.as(:enum)   |
        `struct` >> space? >> brackets(program).as(:struct) |
        (modifier.as(:ident) >> space).repeat(0) >>
        join(
          (ident | namespaced_ident) >> generic?, 
          str('::'), 0).as(:namespaced)
        #join(`in `.absnt? >> ident, space, 1) |
        
      ).as(:base)
    ).as(:type)
  }
  rule(:try_catch) {
    `try` >> space? >> brackets(program.as(:attempt)) >>
    ( space? >> `catch` >> (space >> ident_def | space? >> parens(ident_def)).as(:catch) >> 
      space? >> bracket_body.as(:body)
    ).repeat(1).as(:catches)
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
      ( 
        (
          ident.as(:name) >> colon_is  >>
          type.as(:type) >> spaced?(str(?=) >> space? >> expr.as(:expr))
        ).as(:var_decl) |
      ##multiple decl/initialize
        comma_list(
          (
            comma_list(ident.as(:name) >> parens(comma_list(expr).maybe).maybe.as(:constructor)).as(:names) >>
            colon_is >> type.as(:type)
          ).as(:var_decl)
        ).as(:stmts)
      ) >>
      space? >> semicolon
    )#.as(:var_decl)
  }
  rule(:class_visibility_decl) {
    visibility.as(:visibility_stmt) >> space? >> colon >> eol
  }
  rule(:type_decl) {
    (
      `type` >> space >> (ident >> generic?).as(:name) >>
      colon_is >>
      (class_decl | type).as(:type) >>
      (space? >> str(';') | eol | any.absnt?)
    ).as(:type_decl)
  }
  rule(:import_stmt) {
    `import` >> space >> join(ident, str(?/)).as(:import_pkg)
  }
  rule(:define_stmt) {
    (
      `define` >> space >> ident.as(:name) >>
      (colon_is | space_nonterminal) >> expr.as(:expr) >> eol
    ).as(:define_stmt)
  }
  rule(:include_stmt) {
    (
      `include` >> space >>
      join(( str(?<).as(:std) >> ((alphnum|str(?/)).repeat(1) >> (str('.') >> 
        alphnum.repeat(1,3)).maybe).as(:file) >> str(?>) |
        string.as(:local) 
      ).as(:include_stmt), space? >> comma >> space?, 0) >>
      (eol | space? >> semicolon)
    ).as(:include_stmt)
  }
  rule(:alphnum) { match['A-Za-z0-9_'] }
  rule(:using_stmt) {
    (
      `using` >> space >> (`namespace`.as(:ns) >> space).maybe >>
      comma_list((namespaced_ident | ident).as(:using_namespace)) >>
      (eol | space? >> semicolon)
    ).as(:using_stmt)
  }
  rule(:namespace_decl) {
    (
      `namespace` >> space >> ident.as(:namespace) >> space? >>
      bracket_body.as(:body)
    ).as(:namespace_decl)
  }
  rule(:return_stmt) {
    
      (`return` >> (space >> expr | parens(expr))).as(:return) >> space? >> semicolon
    
  }
  rule(:ctor_decl) {
    (
      `ctor` >> (space >> (namespaced_ident|ident).as(:name)).maybe >> colon_is? >>
      #parens(`void` | ident_defs).as(:args) >> space? >>
      func_sig_args >> space? >>
      join(
        (
          (namespaced_ident|ident).as(:member) >> parens(join(expr, space? >> comma >> space)).as(:args)
        ).as(:initializer),  #  expr.as(:initial_value)),
        space? >> (comma >> space?).maybe
      ).maybe.as(:initializers) >> space? >>
      body.as(:body)
    ).as(:ctor_decl)
  }
  rule(:dtor_decl) {
    (
      `dtor` >> space? >> (colon >> space?).maybe >>
      (specifier.as(:specifier) >> space?).maybe >>
      (parens(spaced?(`void`)) >> space?).maybe >>
      body.as(:body)
    ).as(:dtor_decl)
  }

  rule(:operator) {
    str('&&') | str('||') | str('==') | str('!=') |
    str('<<=')| str('>>=')| str('<=') | str('>=') |
    str('+=') | str('-=') | str('/=') | str('*=') |
    str('|=') | str('&=') |
    str('++') | str('--') | str('<<') | str('>>') |
    str('+')  | str('-')  | str('/')  | str('*')  |
    str('&')  | str('|')  | str('=')  | str('%')  |
    str('<')  | str('>')
  }
  rule(:operator_prefix) {
    str('++') | str('--') | match['!~+\-&*'] |
    ((`new` | `delete`) >> space)
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
      (space? >> str(?<) >> space? | space >> (`of`|`inherits`) >> space) >>
      join(parent_class, space? >> comma >> space?).as(:parents)
    ).maybe >>
    space? >> brackets(program).maybe.as(:body)
  }
  rule(:parent_class) {
    (visibility.as(:vis) >> space).maybe >>
    (namespaced_ident | ident).as(:parent)
  }
  rule(:visibility) {
    `public` | `private` | `protected`
  }


  rule(:oper_decl) {
    (
      `oper` >> space >> 
      (
        `implicit`.as(:name) |
        ((`pre`|`post`).as(:pos) >> space?).maybe >>
          operator.as(:name)
      ) >> colon_is? >> 
      ##((`pre` | `post`).as(:pos) >> space).maybe >>
      ##operator.as(:name) >> colon_is? >>
      #func_sig.as(:sig) >> space? >> body.as(:body)
      func_sig_nosave.present? >> type.as(:sig) >>
      space? >> (body.as(:body) | semicolon)
    ).as(:oper_decl)
  }
  rule(:generic_identifiers) {
    str(?<) >> space? >> comma_list(ident.as(:ident)) >>
    space? >> str(?>)
  }
  rule(:lambda_func) {
    ## [captures](params)->returns{body}
    #[foo,&bar](x: &int)->void{return;}
    (
      str(?[) >> 
        spaced?(
          comma_list(
            str(?&).maybe.as(:ref) >> space? >> ident.as(:name)
          ).maybe
        ).as(:captures) >>
      str(?]) >> 
      spaced?(func_sig_args) >>
      #spaced?(parens(spaced?(ident_defs.as(:params)))) >>
      (str('->') >> space? >> type >> space?).maybe.as(:returns) >>
      bracket_body.as(:body)
    ).as(:lambda_func)
  }
  rule(:func_decl) {
    (
      `func` >> space >> ident.as(:name) >> 
      generic_identifiers.maybe.as(:generics) >> colon_is? >>
      func_sig_nosave.present? >> 
      (specifiers >> space?).maybe >> type.as(:sig) >>
      space? >> (
        body.as(:body) | 
        (str(?=) >> space? >> str('0')).as(:eq_0).maybe >> semicolon
      )
    ).as(:func_decl)
  }
  rule(:func_sig_nosave) { ##func_sig.present? didnt work, but this does..
    (specifiers >> space?).maybe >> parens(`void` | `any` | ident_defs | comma_list(type)) >>
    spaced?(str('->')) >> type
  }
  rule(:func_sig) {
    func_sig_args >> space? >> 
    str('->') >> space? >> type.as(:returns)
  }
  rule(:func_sig_args_anon) {
    parens(
      `void`.as(:void) | 
      `any`  | 
      comma_list(type) 
    ).as(:args)
  }
  rule(:func_sig_args) {
    parens(
      `void`.as(:void) |
      `any`.as(:any)   |
      ident_defs       |
      comma_list(type) 
    ).as(:args)
  }

  rule(:eol) {
    match[' \t'].repeat >> match['\n']
  }

  rule(:access) do str('.*') | str('.') | str('->') end
  rule(:dot_ident) {
    join(ident, access.as(:access)).as(:dot_ident)
  }
  rule(:dot_expr) {
    #join(expr.as(:expr), access.as(:access)).as(:dot_expr)
    join(base_expr.as(:expr), access.as(:access)).as(:dot_expr)
  }
  rule(:func_call_new) {
    (
      str(?<) >> space? >> comma_list(type).as(:generics) >> space? >> str(?>) #>> space_nonterminal.maybe
    ).maybe >>
    (
      str(?!) | 
      parens(comma_list(expr).maybe) | 
      space_nonterminal >> comma_list(expr)
    ).as(:args)
  }
  rule(:func_call) {
    (namespaced_ident | dot_ident | expr).as(:func) >>
    (
      str(?!) |
      parens(comma_list(expr).maybe) |
      space_nonterminal >> comma_list(expr)
    ).as(:args)

    # join(
    #   parens?(
    #     (namespaced_ident | dot_ident).as(:func) >>
    #     (
    #       str(?!) |
    #       parens(comma_list(expr).maybe) |
    #       space_nonterminal >> comma_list(expr)
    #     ).as(:args)
    #   ), access.as(:access), 0
    # ).as(:dot_ident).as(:func_call)
  }
  rule(:space_nonterminal) {
    match[' \t'].repeat(1) >> (str(?\\) >> space).maybe |
    str(?\\) >> space
  }

  rule(:range_for) { #c++11 for range(for(int i : someVec) {...})
    (
      `for` >> space >> ident_def.as(:var) >> space >> `in` >> space >> expr.as(:range) >>
      spaced_body.as(:body)
    ).as(:range_for)
  }

  rule(:conditional) {
    range_for | 
    (
      ( `if`|`elseif`|`while`).as(:kind) >> 
        (space >> expr | space? >> parens(expr)).as(:cond) >>
        spaced_body.as(:body) |
      
        `else`.as(:kind) >> spaced_body.as(:body) |
        
        `for`.as(:kind) >> parens_oder_spacen(
          (var_decl | auto_decl | expr >> space? >> semicolon).as(:l) >>
          space? >> expr.as(:m) >> space? >> semicolon >>
          space? >> expr.as(:r) 
        ) >> spaced_body.as(:body)
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
  rule(:base_expr) {
    (operator_prefix.as(:op) >> space?).maybe.as(:prefix) >>
    (
      float | int | string | char | cast | namespaced_ident | dot_ident | 
      lambda_func |
      brackets(comma_list(expr)).as(:struct_lit) |
      paren_tagged(base_expr, :parens)
    ).as(:expr) >>
    (operator_postfix | sq_bracket_expr).as(:op).maybe.as(:postfix) >>
    (((space >> operator | operator).absent? | str('<<').absent? >> str(?<).present?) >> func_call_new).maybe >>
    #((space? >> operator).absent? >> func_call_new).maybe >> 
    ( space? >> 
      (
        str(??).as(:ternary) >> space? >> expr.as(:true) >> 
        space? >> colon >> space? >> expr.as(:false) |
        operator.as(:infix) >> space? >> expr.as(:right)
      )
    ).maybe
  }
  rule(:expr) {
    #paren_tagged?(base_expr.as(:expr), :parens)
    #base_expr.as(:expr)
    dot_expr
  }
  rule(:sq_bracket_expr) {
    (
      str(?[) >> space? >> expr >> space? >> str(?])
    ).as(:sq_bracket_expr)
  }
  rule(:cast) {
    (
      `cast` >> str(?<) >> type.as(:type) >> str(?>) >> 
      parens(expr.as(:expr))
    ).as(:cast)
  }

  rule(:switch_stmt) {
    (
      `switch` >> (space? >> parens(expr) | space >> expr).as(:expr) >> space? >> 
      brackets(
        (
          (`case` >> space >> comma_list(expr) | `default`).as(:case_) >> space?>>colon>>
          brackets?(program.as(:body)) >> space?
        ).repeat(1)
      ).as(:cases)
    ).as(:switch_stmt)
  }

  rule(:stmt) {
    var_decl | auto_decl | import_stmt | using_stmt | func_decl | 
    try_catch |
    return_stmt | class_visibility_decl | switch_stmt |
    namespace_decl | type_decl | ctor_decl | dtor_decl | oper_decl |
    conditional | lang_section | include_stmt | define_stmt |
    brackets(program.as(:bracket_stmts)) |
    (dot_expr.as(:expr_stmt) >> space? >> semicolon)
    #(expr.as(:expr_stmt) >> space? >> semicolon)
  }
  rule(:bracket_body) {
    brackets(program.maybe)
  }
  rule(:body) {
    bracket_body | stmt
  }
  rule(:spaced_body) {
    (space? >> bracket_body) | (space >> stmt)
  }
  rule(:program) {
    space? >>
    (stmt >> space?).repeat.as(:stmts)
  }

  root :program
end
end