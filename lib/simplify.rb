#!/usr/bin/env ruby
require 'set'

OPTIONAL_REGEX = / _.*?_/

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
    if line[0] != ' '
      currentRule = line[0..-2]

      if nonTerminals.include? currentRule
        puts "Duplicate reference to #{currentRule}"
        exit(1)
      end

      nonTerminals.add(currentRule)
      terminals.delete(currentRule)
    else
      newTerminals = line.gsub('_','').split(' ').reject{|token| nonTerminals.include? token}
      terminals.merge newTerminals
      rules = expand(line).map{|rule| ' ' + rule.strip}
      productionRules[currentRule].concat rules
    end
  end

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
