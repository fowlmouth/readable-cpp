require'parslet'
module ParserMethods
  def `(str) 
    str.split(//).
    map{|c|
      match[c =~ /[A-Za-z]/ ? c.upcase<<c.downcase : c]
    }.
    reduce(:>>)
  end
  def join(rule, on, min = 0) 
    rule >> (on >> rule).repeat(min) 
  end
  def parens(rule) 
    l_paren >> space? >> rule >> space? >> r_paren 
  end
  def comma_list(rule, min = 0) 
    join(rule, space? >> comma >> space?, min)
  end
  def self.included(cls)
    raise <<-end unless cls < Parslet::Parser
Hi there, you included ParserMethods in a class that doth not
derive from Parslet::Parser. Be thou tripping?
    end
    cls.class_eval do
      rule(:l_paren) { str(?() }
      rule(:r_paren) { str(?)) }
      rule(:comma) { str(?,) }
      rule(:colon) { str(?:) }
      rule(:semicolon) { str(?;) }
      rule(:l_bracket) { str(?{) }
      rule(:r_bracket) { str(?}) }
    end
  end
end