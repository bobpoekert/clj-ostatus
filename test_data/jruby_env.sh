#!/bin/sh
alias jruby="PATH=bin:$PATH GEM_HOME=vendor/gem_home GEM_PATH=vendor/gem_home java -jar ~/.m2/repository/org/jruby/jruby-complete/9.1.6.0/jruby-complete-9.1.6.0.jar"
alias rspec="jruby -S rspec"
alias rake="jruby -S rake"
alias bundle="jruby -S bundle"
