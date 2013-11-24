require 'rubygems'
require 'pry-stack_explorer'
require 'pry-coolline'
require 'awesome_print'

AwesomePrint.pry!

AwesomePrint.defaults = {
  :indent => -2,
  :color => {
    :hash  => :pale,
    :class => :white
  }
}
