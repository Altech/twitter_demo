#! /usr/local/bin/ruby
# coding: utf-8

require 'json'

mapping = Hash.new # user_id to name
required_users = Array.new

topn_data = $stdin
mapping_data = File.open('resources/alluserlist','r')

$stdin.each do |l|
  json = JSON.parse(l)
  ids = json["Result"]["Topk"]
  required_users += ids
end

required_users.uniq!
required_users.sort!


mapping_data.each do |l|
  id, name = l.split(/[\s]+/).map(&:strip)
  puts [id,name].join("\t") if required_users.include? id.to_i
end

