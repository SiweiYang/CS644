#!/usr/bin/env ruby
require 'set'

OPTIONAL_REGEX = / \|.*?\|/

PUNCTUATION_SUBSTITUTIONS = {
  '(' => 'SEPARATOR_LPAREN',
  ')' => 'SEPARATOR_RPAREN',
  '{' => 'SEPARATOR_LBRACE',
  '}' => 'SEPARATOR_RBRACE',
  '[' => 'SEPARATOR_LBRACK',
  ']' => 'SEPARATOR_RBRACK',
  ',' => 'SEPARATOR_COMMA',
  '.' => 'SEPARATOR_DOT',
  ';' => 'SEPARATOR_SEMI'
}

def expand(rule)
  return [rule].flatten unless rule =~ OPTIONAL_REGEX

  expanded = rule.dup
  expanded[OPTIONAL_REGEX] = ' ' + rule[OPTIONAL_REGEX][2..-2]

  removed = rule.dup
  removed[OPTIONAL_REGEX] = ''

  return expand(expanded).concat(expand(removed))
end

def main
  lines = ARGF.readlines.map(&:chomp).reject(&:empty?)

  currentRule = nil
  terminals = Set.new
  nonTerminals = Set.new
  productionRules = Hash.new {|this, key| this[key] = [] }

  lines.each do |line|    
    PUNCTUATION_SUBSTITUTIONS.each do |initial, result|
      line.gsub!(initial, result)
    end

    if not line.start_with?(' ')
      currentRule = line[0..-2]

      if nonTerminals.include? currentRule and false
        puts "Duplicate reference to #{currentRule}"
        exit(1)
      end

      nonTerminals.add(currentRule)
      #terminals.delete(currentRule)
    else
      newTerminals = line.gsub('|','').split(' ').reject{|token| nonTerminals.include? token}
      terminals += newTerminals
      rules = expand(line).map{|rule| ' ' + rule.strip}
      productionRules[currentRule].concat rules
    end
  end
  terminals -= nonTerminals

  # Print out terminals
  puts terminals.count
  terminals.each {|terminal| puts terminal}

  # Print out non-terminals
  puts nonTerminals.count
  nonTerminals.each {|nonTerminal| puts nonTerminal}

  # Print out start state (first non-terminal in file)
  puts lines[0][0..-2]

  # Print out production rules
  puts productionRules.values.map(&:length).inject{|sum, n| sum + n}
  productionRules.each do |nonTerminal, rules|
    rules.each do |rule|
      puts "#{nonTerminal}#{rule}"
    end
  end
end

main
