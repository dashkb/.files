require 'rubygems'
require 'awesome_print'

AwesomePrint.pry!

AwesomePrint.defaults = {
  :indent => -2,
  :color => {
    :hash  => :pale,
    :class => :white
  }
}

Pry.config.correct_indent = !ENV['INSIDE_EMACS']
Pry.config.pager = !ENV["INSIDE_EMACS"]